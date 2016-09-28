package ui

import java.util.UUID
import javafx.fxml.{FXMLLoader, Initializable}
import javafx.scene.Parent
import javafx.util.Callback

import akka.actor.{ActorRef, ActorRefFactory, Props}
import ui.ControllerContext.InitControllerContext

import scala.concurrent.ExecutionContext

/**
  * Created by Gábor on 2016.09.28..
  */
trait Controller[V] extends Initializable {

  trait Ui extends (V => Unit)

  type EventReceive = PartialFunction[Event, Ui]

  private[this] var _backgroundExecutor: ExecutionContext = _
  implicit lazy val backgroundExecutor: ExecutionContext = _sideffectExecutor

  private[this] var _sideffectExecutor: ExecutionContext = _
  implicit lazy val sideeffectExecutor: ExecutionContext = _sideffectExecutor


  def ui(f: V => Unit): Ui = new Ui {
    override def apply(view: V): Unit = f(view)
  }
}

object SideEffect {
  def apply(f: => Unit): () => Unit = () => f
}

object ControllerContext {

  type SideEffect = () => Unit
  type EventReceive = PartialFunction[Event, SideEffect]

  case class InitControllerContext(f: (ActorRefFactory, ActorRef) => ControllerContext)

  private def getBean(clazz: Class[_], dependencies: Map[String, ActorRef]): AnyRef = {
    import scala.reflect.runtime.{universe => ru}

    implicit val mirror = ru.runtimeMirror(this.getClass.getClassLoader)

    def getType(clazz: Class[_])(implicit runtimeMirror: ru.Mirror) = runtimeMirror.classSymbol(clazz).toType


    val cla = getType(clazz).typeSymbol.asClass
    val cm = mirror.reflectClass(cla)
    val constructor = getType(clazz).decl(ru.termNames.CONSTRUCTOR).asMethod
    val constructorMethod = cm.reflectConstructor(constructor)
    val args = constructor.asMethod.paramLists.head map { p => (p.name.decodedName.toString, p.typeSignature) }
    val resolvedArgs = args.map {
      case (name, tpe) => dependencies.getOrElse(name, null)
    }

    val ins = constructorMethod.apply(resolvedArgs: _*).asInstanceOf[AnyRef]
    ins
  }

}

class ControllerContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, val backgroundExecutionContext: ExecutionContext, behaviorContext: BehaviorContext) {

  def controllerOf(creator: Props, behavior: ActorRef): ActorRef = {
    val controllerContext = ??? //new ControllerContext(_:ActorRefFactory , _: ActorRef, eventStream, backgroundExecutionContext, behavior)
    val controller = actorRefFactory.actorOf(creator, s"Controller-${UUID.randomUUID().toString}")
    controller ! InitControllerContext(controllerContext)
    controller
  }

  private def componentNameOf(clazz: Class[_]): String = {
    val controllerName = clazz.getCanonicalName
    if (controllerName.endsWith("Controller")) controllerName.substring(0, controllerName.lastIndexOf("Controller"))
    else controllerName
  }

  private[this] def loadImpl[T: Manifest](behavior: Option[ActorRef] = None): (T, Parent) = {
    val controllerResource = {
      val name = {
        val clazz = manifest[T].runtimeClass
        componentNameOf(clazz).replace(".", "/") + ".fxml"
      }
      println("-------" + name)
      getClass.getClassLoader.getResource(name)
    }

    def controllerFactory(loader: FXMLLoader, firstLevel: Boolean): Callback[Class[_], AnyRef] = new Callback[Class[_], AnyRef] {
      override def call(clazz: Class[_]): AnyRef = {

        val behaviorActor = behavior.filter(_ => firstLevel).getOrElse {
          val behaviorName = {
            val componentName = componentNameOf(clazz)
            if(componentName.endsWith("View")) componentName.substring(0, componentName.lastIndexOf("View")) + "Behavior"
            else componentName + "Behavior"
          }
          println("Trying to create behavior for: " + behaviorName)

          val props = Props(Class.forName(behaviorName))
          val actorRef = behaviorContext.behaviorOf(props)
          actorRef
        }

        println("Trying to create controller: " + clazz)

        val dependencies = Map(
          "behavior" -> behaviorActor
        )

        val controllerInstance = ControllerContext.getBean(clazz, dependencies)
        controllerInstance match {
          case c: Controller[_] =>
            println("Initializable")
          case _ =>
            println("Not initilaziable")
        }

        loader.setControllerFactory(controllerFactory(loader, false))

        controllerInstance
      }
    }

    println("resource: " + controllerResource)
    val loader = new FXMLLoader(controllerResource)
    loader.setControllerFactory(controllerFactory(loader, true))
    val parent: Parent = loader.load()
    (loader.getController, parent)
  }

  def load[T: Manifest]: (T, Parent) = {
    loadImpl[T](None)
  }

  def load[T: Manifest](behavior: ActorRef): (T, Parent) = {
    loadImpl[T](Some(behavior))
  }

}
