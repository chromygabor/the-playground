package com.chromy.frpui.core

import monocle.macros.GenLens
import rx.lang.scala.schedulers.{ImmediateScheduler, ComputationScheduler}
import rx.lang.scala.{Subject, Observer}

/**
 * Created by cry on 2015.10.29..
 */
//oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
//o App
//oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
class FrpApp[M](initialState: M, services: Map[Class[_], ServiceBuilder[_]]) {

  def onNext(action: Action): Unit = {
    s.onNext(action)
  }

  private[this] def newContext(model: AppModel, iChannel: Observer[Action]) = new Context {
    def onAction(action: Action): Unit = {
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

  case class AppModel(app: M = initialState, services: Map[String, BaseService] = Map(), uid: Uid = Uid()) extends Model[AppModel] {
    override def children = List(
      Child(GenLens[AppModel](_.app)),
      Child(GenLens[AppModel](_.services))
    )

    override def handle(implicit context: Context): Updater[AppModel] = Updater {
      case ServiceBuilder.ServiceAdded(key, service) => copy(services = services.updated(key, service))
      case _ => this
    }
  }

  private[this] val s = Subject[Action]
  private[this] val render = SideEffectChain[AppModel]()

  private[this] val stream = s.observeOn(ComputationScheduler()).scan(AppModel()) { (model, action) =>
    println(s"======= Action received: $action")
    implicit val context = newContext(model, s)
    model.step(action)
  }.observeOn(ImmediateScheduler()).subscribe({ model =>
    render.update(model).run()
  })

}
