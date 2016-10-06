package ui

import java.net.URL
import java.util.ResourceBundle
import javafx.fxml.{FXMLLoader, Initializable}
import javafx.scene.Parent
import javafx.util.Callback

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import rx.lang.scala.Observer

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

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

class ControllerContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, implicit val backgroundExecutor: ExecutionContext, behaviorContext: BehaviorContext) {


  def controllerOf[A <: Controller : Manifest]: (A, ActorRef) = {
    val clazz = manifest[A].runtimeClass
    val (behaviorActor, fPersistenceId) = {
      val behaviorName = s"${componentNameOf(clazz)}Behavior"

      println("Trying to create behavior for: " + behaviorName)

      val props = Props(Class.forName(behaviorName))
      val actorRef = behaviorContext.behaviorOf(props)
      actorRef -> behaviorContext.persistenceIdOf(actorRef)
    }

    val dependencies = Map(
      "behavior" -> behaviorActor
    )

    val controller = ControllerContext.getBean(clazz, dependencies).asInstanceOf[A]

    def afterInit[T](observer: Observer[(T, Event)]): Unit = {
      //controllerRef ! ViewInitialized(viewInstance, behaviorActor, eventStream, backgroundExecutionContext, sideEffectExecutor = ExecutionContext.fromExecutor(JavaFXExecutor))
    }

    fPersistenceId.onComplete {
      case Success(persistenceId) =>
        controller.setup(actorRefFactory, behaviorActor, eventStream, backgroundExecutor, ExecutionContext.fromExecutor(JavaFXExecutor), persistenceId)
      case Failure(error) => throw error
    }

    controller -> behaviorActor
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

        val (behaviorActor, fPersistenceId) = behavior
          .filter(_ => firstLevel)
          .map(behavior => behavior -> behaviorContext.persistenceIdOf(behavior))
          .getOrElse {
            val behaviorName = s"${componentNameOf(clazz)}Behavior"

            println("Trying to create behavior for: " + behaviorName)

            val props = Props(Class.forName(behaviorName))
            val actorRef = behaviorContext.behaviorOf(props)
            actorRef -> behaviorContext.persistenceIdOf(actorRef)
          }

        val dependencies = Map(
          "behavior" -> behaviorActor
        )

        //        val controllerName = clazz
        //        val props = Props(Class.forName(controllerName))
        val controller = ControllerContext.getBean(clazz, dependencies)

        def afterInit[T](observer: Observer[(T, Event)]): Unit = {
          //controllerRef ! ViewInitialized(viewInstance, behaviorActor, eventStream, backgroundExecutionContext, sideEffectExecutor = ExecutionContext.fromExecutor(JavaFXExecutor))
        }

        controller match {
          case ctrl: Controller =>
            fPersistenceId.onComplete {
              case Success(persistenceId) =>
                ctrl.setup(actorRefFactory, behaviorActor, eventStream, backgroundExecutor, ExecutionContext.fromExecutor(JavaFXExecutor), persistenceId)
              case Failure(error) => throw error
            }

          case _ =>
        }

        loader.setControllerFactory(controllerFactory(loader, false))


        controller
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

trait Controller extends Initializable {

  case class Context(behavior: ActorRef, eventStream: ActorRef, subscriber: ActorRef, backgroundExecutor: ExecutionContext)

  trait Ui extends Runnable

  type EventReceive = PartialFunction[Event, Ui]

  def ui(f: => Unit): Ui = new Ui {
    override def run(): Unit = f
  }

  implicit def backgroundExecutor: ExecutionContext = context.map(_.backgroundExecutor).getOrElse(sys.error("BackgroundExecutor is not set yet"))

  def behavior: ActorRef = context.map(_.behavior).getOrElse(sys.error("BackgroundExecutor is not set yet"))

  private[this] var context: Option[Context] = None

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    context.map(_.subscriber).getOrElse(sys.error("Subscription not set yet")) ! Initialized
  }

  def setup(actorRefFactory: ActorRefFactory, newBehavior: ActorRef, eventStream: ActorRef, backgroundExecutor: ExecutionContext, uiExecutor: ExecutionContext, persistenceId: String): Unit = {
    //Unsubscribe if there is a previous subscription
    context.foreach { case Context(behavior, eventStream, subscriber, _) =>
      eventStream ! Unsubscribe(subscriber)
    }

    println(s"persistenceId: $persistenceId")
    //Create a new subscription
    val newSubscriber = actorRefFactory.actorOf(Props(new Actor {
      override def receive: Receive = {
        case EventEnvelope(sender, event) if sender == behavior && onEvent.isDefinedAt(event) =>
          //println("Envelope in controller")
          val ui = onEvent(event)
          uiExecutor.execute(ui)
        case Initialized =>
          //println("Initialized in controller")
          val ui = onEvent(Initialized)
          uiExecutor.execute(ui)
        case e =>
          //println(s"Controller: $e")
      }
    }))
    eventStream ! Subscribe(newSubscriber)
    context = Some(Context(newBehavior, eventStream, newSubscriber, backgroundExecutor))
    behavior ! Subscribed
  }

  def onEvent: EventReceive
}
