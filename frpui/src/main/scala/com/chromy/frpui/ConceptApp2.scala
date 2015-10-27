package com.chromy.frpui

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.ConceptApp2.ServiceModel
import com.chromy.frpui.core.Model
import com.chromy.frpui.core.Updater.Simple
import com.chromy.frpui.core._
import monocle.macros.GenLens
import rx.lang.scala.{Observer, Subject}

/**
 * Created by cry on 2015.10.15..
 */

case class Service[B <: Model[B]](model: B)
case class ServiceAdded[B <:  Model[B]](serviceName: String, service: Service[B]) extends Action
trait ServiceBuilder[A <: Model[A]] {
  def initialValue: Service[A]
  def name: String
}

object ServiceBuilder {
  def singleton[A <: Model[A] : Manifest](iInitialValue: A): ServiceBuilder[A] = new ServiceBuilder[A] {
    override lazy val initialValue: Service[A] = Service(iInitialValue)

    override lazy val name: String = {
      val m = manifest[A]
      m.runtimeClass.getName
    }
  }
}

object Services {
  lazy val services = Map[String, ServiceBuilder[_]](
    "com.chromy.frpui.ConceptApp2.ServiceModel" -> ServiceBuilder.singleton(ServiceModel())
  )
}

case class Defer[A <: Model[A], V](value: V, f: (V, A) => A) extends Action

object ConceptApp2 extends App {

  def getService[A <: Model[A], B <: Model[B]](uid: Uid): (Context) => A = ???
  
  def newContext(model: AppModel, iChannel: Observer[Action]) = new Context {
    def onAction(action: Action): Unit = {
      iChannel.onNext(action)
    }
    
    override def getService[B <: Model[B] : Manifest](serviceName: String): B = {
      val service = model.services.getOrElse(serviceName, {
        val sb = Services.services.getOrElse(serviceName, throw new IllegalStateException(s"Service was not found in config: $serviceName"))
        val srv = model.services.getOrElse(sb.name, sb.initialValue)
        val m = manifest[B]
        if(m.runtimeClass.isInstance(srv.model)) {
          s.onNext(ServiceAdded(serviceName, srv.asInstanceOf[Service[B]]))
          srv
        } else {
          throw new IllegalStateException("Service builder type doesn't fit")
        }
      })
      service.model.asInstanceOf[B]
    }
  }
  
  case class ServiceModel(uid: Uid = Uid()) extends Model[ServiceModel] {
    def setConfig(configName: String, value: String) = ???
  }
  
  case class MainModel(value: Int, uid: Uid = Uid()) extends Model[MainModel] {
    val service = getService[ServiceModel, MainModel](uid)
  }
  
  case class AppModel(services: Map[String, Service[_]] = Map(), uid: Uid = Uid()) extends Model[AppModel] {
    override def children = AppModel.children

    override val handle: Updater[AppModel] = Simple { 
      case ServiceAdded(name, service) => copy(services.updated(name, service))
    }
  }

  object AppModel {
    val children = List(
      Child(GenLens[AppModel](_.services))
    )
  }

  val s = Subject[Action]
  val initialModel = AppModel()
  val render = SideEffectChain[AppModel]()

  val stream = s.scan(initialModel) { (model, action) =>
    implicit val context = newContext(model, s)
    model.step(action)
  }.subscribe({ model =>
    render.update(model).run()
  })

  
  
  val u1 = Uid()
//  s.onNext(AddItem(u1))
//  s.onNext(IncrementValue(2, u1))
//  s.onNext(IncrementValue(5, u1))

  val u2 = Uid()
//  s.onNext(AddValue(u2))
//  s.onNext(IncrementValue(7, u2))
//  s.onNext(IncrementValue(9, u2))
//  s.onNext(IncrementValue(11, u2))

}
