package ui

import java.net.URL
import java.util.{ResourceBundle, UUID}
import javafx.fxml.{FXMLLoader, Initializable}
import javafx.scene.Parent
import javafx.util.Callback

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props, Stash}

import scala.concurrent.ExecutionContext

/**
  * Created by Gábor on 2016.09.28..
  */
object ControllerContext {

  private def getBean(clazz: Class[_], dependencies: Map[String, ActorRef] = Map.empty): AnyRef = {
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

trait View extends Initializable {
  private var _afterInit: () => Unit = () => ()

  def setAfterInit(f: () => Unit): Unit = _afterInit = f

  override def initialize(location: URL, resources: ResourceBundle): Unit = _afterInit()

}

case class ViewInitialized(view: AnyRef, behavior: ActorRef, eventStream: ActorRef, backgroundExecutor: ExecutionContext, sideEffectExecutor: ExecutionContext)

class ControllerContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, val backgroundExecutionContext: ExecutionContext, behaviorContext: BehaviorContext) {

  def controllerOf(creator: Props, behavior: ActorRef): ActorRef = {
    val controller = actorRefFactory.actorOf(creator, s"Controller-${UUID.randomUUID().toString}")
    controller
  }

  private def componentNameOf(clazz: Class[_]): String = {
    val clazzName = clazz.getCanonicalName
    if (clazzName.endsWith("Controller")) clazzName.substring(0, clazzName.lastIndexOf("Controller"))
    else if (clazzName.endsWith("View")) clazzName.substring(0, clazzName.lastIndexOf("View"))
    else if (clazzName.endsWith("Behavior")) clazzName.substring(0, clazzName.lastIndexOf("Behavior"))
    else clazzName
  }

  private[this] def loadImpl[T: Manifest](behavior: Option[ActorRef] = None): Parent = {
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
          val behaviorName = s"${componentNameOf(clazz)}Behavior"

          println("Trying to create behavior for: " + behaviorName)

          val props = Props(Class.forName(behaviorName))
          val actorRef = behaviorContext.behaviorOf(props)
          actorRef
        }



        val dependencies = Map(
          "behavior" -> behaviorActor
        )

        val controllerName = s"${componentNameOf(clazz)}Controller"
        val props = Props(Class.forName(controllerName))
        val controllerRef = actorRefFactory.actorOf(props)

        println("Trying to create view: " + clazz)
        val viewInstance = ControllerContext.getBean(clazz)

        def afterInit(): Unit = {
          controllerRef ! ViewInitialized(viewInstance, behaviorActor, eventStream, backgroundExecutionContext, sideEffectExecutor = ExecutionContext.fromExecutor(JavaFXExecutor))
        }

        viewInstance match {
          case v: View => v.setAfterInit(afterInit)
          case _ =>
        }

        loader.setControllerFactory(controllerFactory(loader, false))


        viewInstance
      }
    }

    println("resource: " + controllerResource)
    val loader = new FXMLLoader(controllerResource)
    loader.setControllerFactory(controllerFactory(loader, true))
    val parent: Parent = loader.load()
    parent
  }

  def load[T: Manifest]: Parent = {
    loadImpl[T](None)
  }

  def load[T: Manifest](behavior: ActorRef): Parent = {
    loadImpl[T](Some(behavior))
  }

}

trait Controller[V <: View] extends Actor with Stash {

  trait Ui extends (V => Unit)

  type EventReceive = PartialFunction[Event, Ui]

  private[this] var _backgroundExecutor: ExecutionContext = _
  implicit lazy val backgroundExecutor: ExecutionContext = _sideffectExecutor

  private[this] var _sideffectExecutor: ExecutionContext = _
  implicit lazy val sideeffectExecutor: ExecutionContext = _sideffectExecutor

  private[this] var _behavior: ActorRef = _
  implicit lazy val behavior = _behavior

  def ui(f: V => Unit): Ui = new Ui {
    override def apply(view: V): Unit = f(view)
  }

  def receive: Receive = {
    case ViewInitialized(view: AnyRef, behavior: ActorRef, eventStream: ActorRef, backgroundExecutor, uiExecutor) if view.isInstanceOf[V] =>
      _backgroundExecutor = backgroundExecutor
      _sideffectExecutor = uiExecutor
      _behavior = behavior
      eventStream ! Subscribe(self)

      unstashAll()
      context.become(initialized(view.asInstanceOf[V], behavior))
      self ! Initialized
    case ViewInitialized(_, _, _, _, _) =>
      throw new IllegalStateException("Wrong type of view")
    case _ => stash()
  }

  def initialized(view: V, behavior: ActorRef): Receive = {
    case EventEnvelope(`behavior`, e) if onEvent.isDefinedAt(e) =>
      val ui = onEvent(e)
      sideeffectExecutor.execute(new Runnable {
        override def run(): Unit = ui(view)
      })
    case e@Initialized if onEvent.isDefinedAt(e) =>
      val ui = onEvent(e)
      sideeffectExecutor.execute(new Runnable {
        override def run(): Unit = ui(view)
      })
    case e =>
      println(s"$e is not defined")
  }

  def onEvent: EventReceive
}
