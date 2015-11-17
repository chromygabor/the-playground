package com.chromy.frpui.fw.core

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

  protected val aggregatedRoot = Subject[Event]
  
  val onNext: Event => Unit = { action =>
    aggregatedRoot.onNext(action)
  }

  protected def newContext(model: AppModel) = new Context {
    def onAction(action: Event): Unit = {
      FrpApp.this.onNext(action)
    }

    override def getService[B: Manifest]: B = {
      val m = manifest[B]
      val clazz = m.runtimeClass
      val serviceName = m.runtimeClass.getName

      val sb = services.getOrElse(clazz, throw new IllegalStateException(s"Service was not found in config: $serviceName"))
      val serviceKey = sb.key

      val service = model.services.getOrElse(serviceKey, {
        if (m.runtimeClass.isAssignableFrom(sb.clazz)) {
          val srv = sb.initialValue.asInstanceOf[BaseService].step(Init)(this).asInstanceOf[BaseService]
          aggregatedRoot.onNext(ServiceBuilder.ServiceAdded(serviceKey, srv))
          srv
        } else {
          throw new IllegalStateException("Service builder type doesn't fit")
        }
      })
      service.api(this).asInstanceOf[B]
    }
  }

  protected case class AppModel(app: M = state, services: Map[String, BaseService] = Map(), uid: Uid = Uid()) extends Model[AppModel] {
    override lazy val children = List(
      Child(GenLens[AppModel](_.services)),
      Child(GenLens[AppModel](_.app))
    )

    override def handle(implicit context: Context): EventHandler[AppModel] = EventHandler {
      case ServiceBuilder.ServiceAdded(key, service) => copy(services = services.updated(key, service))
      case _ => this
    }
  }

  protected val modelStream = Subject[AppModel]

  private[this] val stream = aggregatedRoot.observeOn(updateScheduler).scan(AppModel()) { (model, action) =>
    println(s"======= Action received: $action")
    implicit val context = newContext(model)
    model.step(action)
  }
  stream.subscribe( {model => modelStream.onNext(model)})
  
}

object StartApp extends Event //It starts the loop

class JavaFxApp[M <: BaseModel](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty,
                   updateScheduler: Scheduler = ComputationScheduler(),
                   renderScheduler: Scheduler = ComputationScheduler(),
                   sideEffectScheduler: Scheduler, f: (Context, SideEffectChain[M], M) => SideEffect) extends FrpApp[M](state, services, updateScheduler)  {

  private val render: SideEffectChain[M] = SideEffectChain[M]

  val sideEffectStream = modelStream.observeOn(renderScheduler).drop(2).map { model =>
    val context = newContext(model)
    render.update(model.app, context)
  }.observeOn(sideEffectScheduler).subscribe({ _.run() })


  val initSideEffect = modelStream.observeOn(renderScheduler).drop(1).take(1).map { model =>
    val context = newContext(model)
    f(context, render, model.app)
  }.observeOn(sideEffectScheduler).subscribe({ _.run() })

  onNext(StartApp)
}

object JavaFxApp {
  def apply[M <: BaseModel](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty,
            updateScheduler: Scheduler = ComputationScheduler(),
            renderScheduler: Scheduler = ComputationScheduler(),
            sideEffectScheduler: Scheduler)(f: (Context, SideEffectChain[M], M) => SideEffect): JavaFxApp[M] = new JavaFxApp[M](state, services, updateScheduler, renderScheduler, sideEffectScheduler, f) 
}
