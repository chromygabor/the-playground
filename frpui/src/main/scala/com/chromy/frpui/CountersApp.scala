package com.chromy.frpui.fw.core

import monocle.macros.GenLens
import rx.lang.scala.schedulers.{ImmediateScheduler, ComputationScheduler}
import rx.lang.scala.{Scheduler, Subject, Observer}

/**
 * Created by cry on 2015.10.29..
 */
//oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
//o App
//oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
class FrpApp[M](state: M, services: Map[Class[_], ServiceBuilder[_]] = Map.empty, updateScheduler: Scheduler = ComputationScheduler(), renderScheduler: Scheduler = ComputationScheduler(), sideEffectScheduler: Scheduler) {

  val onNext: Event => Unit = { action =>
    s.onNext(action)
  }

  private[this] def newContext(model: AppModel, iChannel: Observer[Event]) = new Context {
    def onAction(action: Event): Unit = {
      iChannel.onNext(action)
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
          s.onNext(ServiceBuilder.ServiceAdded(serviceKey, srv))
          srv
        } else {
          throw new IllegalStateException("Service builder type doesn't fit")
        }
      })
      service.api(this).asInstanceOf[B]
    }
  }

  private case class AppModel(app: M = state, services: Map[String, BaseService] = Map(), uid: Uid = Uid()) extends Model[AppModel] {
    override lazy val children = List(
      Child(GenLens[AppModel](_.app)),
      Child(GenLens[AppModel](_.services))
    )

    override def handle(implicit context: Context): EventHandler[AppModel] = EventHandler {
      case ServiceBuilder.ServiceAdded(key, service) => copy(services = services.updated(key, service))
      case _ => this
    }
  }

  private[this] val s = Subject[Event]
  private[this] val appRender = SideEffectChain[AppModel]()

  private[this] val stream = s.observeOn(updateScheduler).scan(AppModel()) { (model, action) =>
    println(s"======= Action received: $action")
    implicit val context = newContext(model, s)
    model.step(action)
  }.drop(1).observeOn(renderScheduler).map { model =>
    appRender.update(model)
  }.observeOn(sideEffectScheduler).subscribe({ _.run() })

  val render: SideEffectChain[M] = appRender.map(_.app)
  val initialState:M = state
  
  
}
