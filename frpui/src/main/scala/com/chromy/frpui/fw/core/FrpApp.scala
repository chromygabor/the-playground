package com.chromy.frpui.fw.core

import com.chromy.frpui.RendererChain.RendererChain
import com.chromy.frpui.{Renderer, RendererChain}
import monocle.macros.GenLens
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Scheduler, Subject}

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
                         updateScheduler: Scheduler = ComputationScheduler()) {

  private case object InitServices extends Event 
  
  protected val aggregatedRoot = Subject[Event]
  
  val onNext: Event => Unit = { action =>
    aggregatedRoot.onNext(action)
  }

  protected def updateContext(model: AppModel) = new UpdateContext {
    def onAction(action: Event): Unit = {
      FrpApp.this.onNext(action)
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

    override def publish(event: Event): Unit = ???
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
  private[this] val stream = aggregatedRoot.observeOn(updateScheduler).scan(initialValue) { (model, action) =>
    println(s"======= Action received: $action")
    val context = updateContext(model)
    model.step(action)(context)
  }
  
  stream.subscribe( {model => modelStream.onNext(model)})

  aggregatedRoot.onNext(InitServices)
  
}

object StartApp extends Event //It starts the loop

class JavaFxApp[M <: BaseModel](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty,
                   updateScheduler: Scheduler = ComputationScheduler(),
                   renderScheduler: Scheduler = ComputationScheduler(),
                   sideEffectScheduler: Scheduler, f: (RenderContext, RendererChain[M], M) => SideEffect) extends FrpApp[M](state, services, updateScheduler)  {

  private val render: RendererChain[M] = RendererChain[M]

  def rendererContext(model: AppModel): RenderContext = new RenderContext {
    override def updateContext: UpdateContext = JavaFxApp.this.updateContext(model)

    override def subscribeToService[B <: BaseService : Manifest](renderer: Renderer[B, RenderContext]): Unit = ???
  }
  
  val sideEffectStream = modelStream.observeOn(renderScheduler).drop(2).map { model =>
    val context = rendererContext(model)
    render.update(model.app, context)
  }.observeOn(sideEffectScheduler).subscribe({ _.run() })


  val initSideEffect = modelStream.observeOn(renderScheduler).drop(1).take(1).map { model =>
    val context = rendererContext(model)
    f(context, render, model.app)
  }.observeOn(sideEffectScheduler).subscribe({ _.run() })

  onNext(StartApp)
}

object JavaFxApp {
  def apply[M <: BaseModel](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty,
            updateScheduler: Scheduler = ComputationScheduler(),
            renderScheduler: Scheduler = ComputationScheduler(),
            sideEffectScheduler: Scheduler)(f: (RenderContext, RendererChain[M], M) => SideEffect): JavaFxApp[M] = new JavaFxApp[M](state, services, updateScheduler, renderScheduler, sideEffectScheduler, f) 
}
