package com.chromy.reactiveui.core

import com.chromy.reactiveui.core.misc.SideEffectChain$
import rx.lang.scala.{Observable, Observer}

import scala.collection.mutable.{WeakHashMap => WMap}
import scala.concurrent.ExecutionContext

/**
 * Created by cry on 2015.09.13..
 */

case class Service[A <: BaseComponent](name: String, context: Context[A#ModelType], service: A) {
  def initialState = context.initialState
  def update(action: Action, model: ComponentModel): A#ModelType = {
    context.chain.update(action, model.asInstanceOf[A#ModelType])
  }
}

trait ServiceBuilder[A <: BaseComponent] {
  val initialState: A#ModelType
  def apply(context: Context[A#ModelType]): A
}

trait ServiceAware {
  protected val servicesContextMapper: ContextMapper[Map[String, _ <: ComponentModel]]
  protected val serviceDefs: Map[String, ServiceBuilder[_ <: BaseComponent]]

  private[this] val services = WMap.empty[BaseComponent, Service[_ <: BaseComponent]]

  private[this] val serviceSubscriber: (Action, Map[String, ComponentModel], Map[String, ComponentModel]) => Map[String, ComponentModel] = { (action, _, model) =>

    // Map wich contains all the updated models of which are both in the Repository and the model
    // We eliminated all models which are missing in Repository
    val nm: List[(String, ComponentModel)] = model.map { case (serviceName, serviceState) =>
      val oService = services.find {
        case (_, Service(`serviceName`, _, _)) => true
        case _ => false
      }
      for {
        (_, service) <- oService
      } yield {
        val newState = service.update(action, serviceState)
        (serviceName, newState.asInstanceOf[ComponentModel])
      }
    }.filter(_.isDefined).map(_.get).toList

    //Map which are in the repository but not in the model
    val m: List[(String, ComponentModel)] = Map(services.toList: _*).filter { case (_, service@Service(serviceName, _, _)) => model.get(serviceName).isEmpty }.map { case (_, service@Service(serviceName, _, _)) =>
      val newState = service.update(action, service.initialState).asInstanceOf[ComponentModel]
      (serviceName, newState)
    }.toList

    (nm ++ m).toMap
  }

  private[this] lazy val serviceContext = servicesContextMapper(serviceSubscriber)

  def service[A <: BaseComponent : Manifest]: A = {
    val m = manifest[A]

    val serviceName = m.runtimeClass.getName

    val oService = services.find {
      case (_, Service(`serviceName`, _, _)) => true
      case _ => false
    } map {
      _._2
    }

    oService.getOrElse {
      val sb2 = serviceDefs.getOrElse(serviceName, throw new IllegalArgumentException(s"There is no configured service with name: $serviceName"))
      if (!sb2.isInstanceOf[ServiceBuilder[A]]) throw new IllegalArgumentException(s"The service was not configured properly (the builder not belonging to the service): $serviceName")

      val sb = sb2.asInstanceOf[ServiceBuilder[A]]

      val localContext = new Context[A#ModelType] {
        override val changes = serviceContext.changes.filter {
          _.contains(serviceName)
        }.map { in => in(serviceName).asInstanceOf[A#ModelType] }

        override val chain: UpdateChain[A#ModelType] = UpdateChain[A#ModelType]()

        override val channel: Observer[Action] = serviceContext.channel

        override val backgroundExecutor: ExecutionContext = serviceContext.backgroundExecutor
        override val initialState = sb.initialState
      }
      val service = sb(localContext)
      services(service) = Service(serviceName, localContext, service)
      services(service)
    }.service.asInstanceOf[A]
  }
}
object Singleton {
  def apply[A <: ComponentModel](iInitialState: A)(create: (ContextMapper[A], A) => A#ComponentType): ServiceBuilder[A#ComponentType] = new ServiceBuilder[A#ComponentType] {
    private[this] var _instance = Option.empty[A#ComponentType]
    type ModelType = A
    type ComponentType = A#ComponentType

    override val initialState: A#ComponentType#ModelType = iInitialState.asInstanceOf[A#ComponentType#ModelType]

    override def apply(context: Context[A#ComponentType#ModelType]): A#ComponentType = {
      _instance.getOrElse {
        val component = create(context.mapper.asInstanceOf[ContextMapper[A]], initialState.asInstanceOf[A])
        _instance = Some(component)
        _instance.get
      }
    }
  }
}
