package com.chromy.frpui.fw.core

import java.io.{PrintWriter, FileOutputStream, StringWriter}

import com.chromy.frpui.fw.javafx.RendererChain.RendererChain
import com.chromy.frpui.fw.javafx.{Renderer, RendererChain}
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.typesafe.scalalogging.LazyLogging
import monocle.macros.GenLens
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Observer, Scheduler, Subject}

/**
 * Created by cry on 2015.10.29..
 */

object FrpApp {
  def apply[M](state: M, 
               services: Map[Class[_], ServiceBuilder[_]] = Map.empty, 
               updateScheduler: Scheduler = ComputationScheduler(), 
               renderScheduler: Scheduler = ComputationScheduler(), 
               sideEffectScheduler: Scheduler): FrpApp[M] = new FrpApp[M](state, services, updateScheduler) {}
}

//oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
//o App
//oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
abstract class FrpApp[M](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty, 
                         updateScheduler: Scheduler = ComputationScheduler()) extends LazyLogging {

  private case object InitServices extends Event 
  
  def offerHistory(events: Seq[Event]): Unit = {
    events.foreach(innerStream.onNext)
  } 
  
  def !(event: Event): Unit = {
    aggregatedRoot.onNext(event)
  }
  
  private val innerStream = Subject[Event]
  private val aggregatedRoot = Subject[Event]()


  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.enableDefaultTyping(ObjectMapper.DefaultTyping.NON_FINAL, JsonTypeInfo.As.WRAPPER_OBJECT);
  val eventout = new PrintWriter(new FileOutputStream("events.txt"))
  
  val arSubscrtiption = aggregatedRoot.subscribe(new Observer[Event] {
    override def onNext(event: Event): Unit = {
      event match {
        case e: PersistableEvent =>
          val output = mapper.writeValueAsString(event)
          eventout.println(output)
          eventout.flush()
          
        case _ =>
      }
      innerStream.onNext(event)
    }

    override def onCompleted(): Unit = {
      super.onCompleted()
    }

    override def onError(error: Throwable): Unit = {
      error.printStackTrace()
    }
  })
  
  protected def updateContext(model: AppModel) = new UpdateContext {
    def !(action: Event): Unit = {
      aggregatedRoot.onNext(action)
    }

    override def getService[B: Manifest]: B = {
      val m = manifest[B]
      val clazz = m.runtimeClass
      val serviceName = m.runtimeClass.getName

      val sb = services.getOrElse(clazz, throw new IllegalStateException(s"Service was not found in config: $serviceName"))
      val serviceKey = sb.key

      val service = model.services.getOrElse(serviceKey, throw new IllegalStateException(s"Service not found with key: $serviceKey"))
      service.api(this).asInstanceOf[B]
    }

  }

  
  protected case class AppModel(app: M = state, services: Map[String, BaseService] = Map(), uid: Uid = Uid()) extends Model[AppModel] {
    override lazy val children = List(
      Child(GenLens[AppModel](_.services)),
      Child(GenLens[AppModel](_.app))
    )

    override def handle(implicit context: UpdateContext): EventHandler[AppModel] = EventHandler {
      case InitServices =>
        val newServices =  this.services.map { case (clazz, service) =>
          clazz -> service.step(Init).asInstanceOf[BaseService]
        }
        copy(services = newServices)
      case _ => this
    }
  }

  val initialValue = services.foldLeft(AppModel()) { case (appModel, (clazz, serviceBuilder)) =>
    val initialValue = serviceBuilder.initialValue.asInstanceOf[BaseService]
    val newServices = appModel.services.updated(serviceBuilder.key, initialValue)
    appModel.copy(services = newServices)
  }
  
  protected val modelStream = Subject[AppModel]

  //Update side
  private[this] val stream = innerStream.observeOn(updateScheduler).scan(initialValue) { case (model, action) =>
    logger.debug(s"======= Action received: $action")
    val context = updateContext(model)
    model.step(action)(context)
  }
  
  stream.subscribe( { model =>
    modelStream.onNext(model)
  })

  innerStream.onNext(InitServices)
  
}

object StartApp extends Event //It starts the loop

class JavaFxApp[M <: BaseModel](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty,
                   updateScheduler: Scheduler = ComputationScheduler(),
                   renderScheduler: Scheduler = ComputationScheduler(),
                   sideEffectScheduler: Scheduler, f: (RenderContext, RendererChain[M], M) => SideEffect) extends FrpApp[M](state, services, updateScheduler)  {

  private val render: RendererChain[M] = RendererChain[M]
  private val serviceRender: RendererChain[Map[String, BaseService]] = RendererChain[Map[String, BaseService]]

  def rendererContext(model: AppModel): RenderContext = new RenderContext {
    override def updateContext: UpdateContext = JavaFxApp.this.updateContext(model)

    override def subscribeToService[B <: BaseService : Manifest](renderer: Renderer[B, RenderContext]): Unit = {
      val m = manifest[B]
      val clazz = m.runtimeClass
      val serviceName = m.runtimeClass.getName

      val sb = services.getOrElse(clazz, throw new IllegalStateException(s"Service was not found in config: $serviceName"))
      val serviceKey = sb.key
      val r = serviceRender.keyOption[String, BaseService](serviceKey).asInstanceOf[RendererChain[B]]
      r.subscribe(renderer)
    }
  }
  
  val sideEffectStream = modelStream.observeOn(renderScheduler).drop(2).map { model =>
    val context = rendererContext(model)
    serviceRender.update(model.services, context)
    render.update(model.app, context)
  }.observeOn(sideEffectScheduler).subscribe({ _.run() })


  val initSideEffect = modelStream.observeOn(renderScheduler).drop(1).take(1).map { model =>
    val context = rendererContext(model)
    serviceRender.update(model.services, context)
    f(context, render, model.app)
  }.observeOn(sideEffectScheduler).subscribe({ _.run() })

  offerHistory(List(StartApp))
}

object JavaFxApp {
  def apply[M <: BaseModel](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty,
            updateScheduler: Scheduler = ComputationScheduler(),
            renderScheduler: Scheduler = ComputationScheduler(),
            sideEffectScheduler: Scheduler)(f: (RenderContext, RendererChain[M], M) => SideEffect): JavaFxApp[M] = new JavaFxApp[M](state, services, updateScheduler, renderScheduler, sideEffectScheduler, f) 
}
