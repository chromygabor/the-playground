package com.chromy.frpui

import com.chromy.frpui.ConceptApp2.ServiceModel
import com.chromy.frpui.core.{Model, _}
import monocle.macros.GenLens
import rx.lang.scala.{Observer, Subject}

/**
 * Created by cry on 2015.10.15..
 */

trait Service[B <: BaseModel] extends BaseModel {
  type M = B
}

case class ServiceAdded[B <: Service[B]](serviceName: String, service: Service[B]) extends Action

trait ServiceBuilder[A <: Service[A]] {
  def initialValue: Service[A]

  def name: String
}

object ServiceBuilder {
  def singleton[A <: Service[A] : Manifest](iInitialValue: A): ServiceBuilder[A] = new ServiceBuilder[A] {
    override lazy val initialValue: Service[A] = iInitialValue

    override lazy val name: String = {
      val m = manifest[A]
      m.runtimeClass.getName
    }
  }
}

object Services {
  lazy val services = Map[String, ServiceBuilder[_]](
    "com.chromy.frpui.ConceptApp2$ServiceModel" -> ServiceBuilder.singleton(ServiceModel())
  )
}

case class Defer[A <: Model[A], V](value: V, f: (V, A) => A) extends Action

object ConceptApp2 extends App {

  def newContext(model: AppModel, iChannel: Observer[Action]) = new Context {
    def onAction(action: Action): Unit = {
      iChannel.onNext(action)
    }

    override def getService[B <: Service[B] : Manifest]: B = {
      val m = manifest[B]
      val serviceName = m.runtimeClass.getName
      val sb = Services.services.getOrElse(serviceName, throw new IllegalStateException(s"Service was not found in config: $serviceName"))
      val serviceKey = sb.name

      val service = model.services.getOrElse(serviceKey, {
        if (m.runtimeClass.isInstance(sb.initialValue)) {
          val srv = sb.initialValue.asInstanceOf[B].step(Init)(this)
          s.onNext(ServiceAdded(serviceKey, srv))
          srv
        } else {
          throw new IllegalStateException("Service builder type doesn't fit")
        }
      })
      service.asInstanceOf[B]
    }
  }

  case class ServiceModel(uid: Uid = Uid()) extends Service[ServiceModel] {
    def setConfig(configName: String, value: String) = ???

    override def handle(context: Context): Updater[ServiceModel] = Updater{
      case Init =>
        println(s"Service initialized")
        this
    }
  }

  case class MainModel(value: Int = 0, uid: Uid = Uid()) extends Model[MainModel] {
    override def handle(context: Context): Updater[MainModel] = Updater {
      case Init =>
        println(s"MainModel initialized")
        val s = context.getService[ServiceModel]
        this
    }
  }

  case class AppModel(app: MainModel = MainModel(), services: Map[String, Service[_]] = Map(), uid: Uid = Uid()) extends Model[AppModel] {
    override def children = AppModel.children

    override def handle(context: Context): Updater[AppModel] = Updater {
      case ServiceAdded(name, service) => copy(services = services.updated(name, service))
    }
  }

  object AppModel {
    val children = List(
      Child(GenLens[AppModel](_.app)),
      Child(GenLens[AppModel](_.services))
    )
  }

  val s = Subject[Action]
  val initialModel = AppModel()
  val render = SideEffectChain[AppModel]()

  val stream = s.scan(initialModel) { (model, action) =>
    println(s"======= Action received: $action")
    implicit val context = newContext(model, s)
    model.step(action)
  }.subscribe({ model =>
    render.update(model).run()
  })


  val u1 = Uid()
  s.onNext(Init)
  // s.onNext(AddItem(u1))
  // s.onNext(IncrementValue(2, u1))
  // s.onNext(IncrementValue(5, u1))

  val u2 = Uid()
  //  s.onNext(AddValue(u2))
  //  s.onNext(IncrementValue(7, u2))
  //  s.onNext(IncrementValue(9, u2))
  //  s.onNext(IncrementValue(11, u2))

}
