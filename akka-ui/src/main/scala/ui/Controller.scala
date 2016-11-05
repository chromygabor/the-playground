package ui

import java.net.URL
import java.util.ResourceBundle
import javafx.fxml.{FXMLLoader, Initializable}
import javafx.scene.Parent
import javafx.util.Callback

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import ui.ControllerContext.Subscribed

import scala.concurrent.duration.Duration
import scala.concurrent.{Promise, Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by Gábor on 2016.09.28..
  */
object ControllerContext {

  case object Subscribed extends Command

  private def createController(clazz: Class[_], dependencies: Map[String, ActorRef] = Map.empty): AnyRef = {
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

class ControllerContext(val behaviorContext: BehaviorContext) extends LazyLogging {


  implicit def backgroundExecutor = behaviorContext.backgroundExecutor

  def subscribeTo(behaviorId: BehaviorId)(f: PartialFunction[Any, Unit]): ActorRef = {
    val subscriber = behaviorContext.actorRefFactory.actorOf(Props(new Actor {
      override def receive: Receive = f
    }))
    behaviorContext.subscribe(subscriber)
    subscriber
  }

  def unsubscribe(oldSubscriber: ActorRef): Unit = ???

  private[this] val controllerContext = this

  val uiExecutor = ExecutionContext.fromExecutor(JavaFXExecutor)

  private def componentNameOf(clazz: Class[_]): String = {
    val clazzName = clazz.getCanonicalName
    if (clazzName.endsWith("Controller")) clazzName.substring(0, clazzName.lastIndexOf("Controller"))
    else if (clazzName.endsWith("View")) clazzName.substring(0, clazzName.lastIndexOf("View"))
    else if (clazzName.endsWith("Behavior")) clazzName.substring(0, clazzName.lastIndexOf("Behavior"))
    else clazzName
  }

  private[this] def loadImpl[T: Manifest](behavior: Option[BehaviorId] = None): Parent = {
    val controllerResource = {
      val name = {
        val clazz = manifest[T].runtimeClass
        componentNameOf(clazz).replace(".", "/") + ".fxml"
      }
      getClass.getClassLoader.getResource(name)
    }

    def controllerFactory(loader: FXMLLoader, firstLevel: Boolean): Callback[Class[_], AnyRef] = new Callback[Class[_], AnyRef] {
      override def call(clazz: Class[_]): AnyRef = {

        val fBehaviorId = behavior
          .filter(_ => firstLevel)
          .map(Future.successful _)
          .getOrElse {
            val behaviorName = s"${componentNameOf(clazz)}Behavior"

            logger.debug("Trying to create behavior for: " + behaviorName)

            val behaviorId = BehaviorId()
            behaviorContext.create(behaviorId, Props(Class.forName(behaviorName)))
              .map(_ => behaviorId)
          }

        val controller = ControllerContext.createController(clazz)

        controller match {
          case ctrl: Controller =>
            fBehaviorId.onComplete {
              case Success(behaviorId) =>
                ctrl.setup(controllerContext, behaviorId)
              case Failure(error) => throw error
            }

          case _ =>
        }

        loader.setControllerFactory(controllerFactory(loader, false))
        controller
      }
    }

    logger.debug("resource: " + controllerResource)
    val loader = new FXMLLoader(controllerResource)
    loader.setControllerFactory(controllerFactory(loader, true))
    val parent: Parent = loader.load()
    parent.setUserData(behaviorContext)
    parent
  }

  def load[T: Manifest]: Parent = {
    loadImpl[T](None)
  }

  def load[T: Manifest](behaviorId: BehaviorId): Parent = {
    loadImpl[T](Some(behaviorId))
  }

}

trait Controller extends Initializable with LazyLogging {

  trait Ui extends Runnable

  lazy val log = logger

  type EventReceive = PartialFunction[Event, Ui]

  def ui(): Ui = new Ui {
    override def run(): Unit = ()
  }

  def ui(f: => Unit): Ui = new Ui {
    override def run(): Unit = f
  }

  implicit def backgroundExecutor: ExecutionContext = context.map { case (controllerContext, _, _, _) => controllerContext.behaviorContext.backgroundExecutor }.getOrElse(sys.error("BackgroundExecutor is not set yet"))

  implicit def timeout: Timeout = context.map { case (controllerContext, _, _, _) => controllerContext.behaviorContext.timeout }.getOrElse(sys.error("Timeout is not set yet"))

  implicit def behaviorContext: BehaviorContext = context.map { case (controllerContext, _, _, _) => controllerContext.behaviorContext }.getOrElse(sys.error("BehaviorContext is not set yet"))

  private def controllerContext: ControllerContext = context.map { case (controllerContext, _, _, _) => controllerContext }.getOrElse(sys.error("BehaviorContext is not set yet"))

  private[this] def behaviorId: BehaviorId = context.map { case (_, _, behaviorId, _) => behaviorId }.getOrElse(sys.error("BehaviorId is not set yet"))

  protected def behavior: BehaviorRef = context.map { case (_, _, _, behaviorRef) => behaviorRef }.getOrElse(sys.error("BehaviorRef is not set yet"))

  private[this] var context: Option[(ControllerContext, ActorRef, BehaviorId, BehaviorRef)] = None

  def load[T: Manifest](behaviorId: BehaviorId): Parent = controllerContext.load[T](behaviorId)

  private[this] val initialized = Promise[Unit]()

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    initialized.success(())
    //context.map { case (_, subscriber, _, _) => subscriber }.getOrElse(sys.error("Subscription not set yet")) ! Initialized
  }

  def setup(newControllerContext: ControllerContext, behaviorId: BehaviorId): Unit = {

    //Create a new subscription
    val newSubscriber = newControllerContext.subscribeTo(behaviorId) {
      case Envelope(envelopeBehaviorId, event, _) if envelopeBehaviorId == behaviorId && onEvent.isDefinedAt(event) =>
        //logger.debug(s"Envelope received by controller: $event")
        Future(onEvent(event)).foreach(newControllerContext.uiExecutor.execute(_))
      case Initialized =>
        Future(onEvent(Initialized)).foreach(newControllerContext.uiExecutor.execute(_))
      case e =>
        //logger.debug(s"$e is not defined in this controller")
    }

    newControllerContext.behaviorContext.behaviorOf(behaviorId).onComplete {
      case Success(behaviorRef) =>
        //Unsubscribe if there is a previous subscription
        context.foreach { case (oldControllerContext, oldSubscriber, _, _) =>
          oldControllerContext.unsubscribe(oldSubscriber)
        }
        context = Some((newControllerContext, newSubscriber, behaviorId, behaviorRef))
        initialized.future.foreach { _ =>
          log.debug(s"Start replaying for $behaviorId")
          newControllerContext.behaviorContext.replay({ envelope =>
            val p = envelope.sender == behaviorId
            log.debug(s"$envelope, $p")
            p
          }, newSubscriber)
        }
      case Failure(error) => throw error
    }(newControllerContext.backgroundExecutor)
  }

  def onEvent: EventReceive
}
