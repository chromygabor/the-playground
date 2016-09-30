package ui

import java.net.URL
import java.util.concurrent.Executor
import java.util.{ResourceBundle, UUID}
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.control.{Button, Label}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.util.Callback

import akka.actor._
import ui.BehaviorContext.InitBehaviorContext

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by chrogab on 2016.09.13..
  */

trait Command

trait Event

case object Init extends Command

case object Initialized extends Event

case object DummyEvent extends Event

case class EventEnvelope(sender: ActorRef, event: Event) extends Event {
  val uid = UUID.randomUUID().toString

  override def toString(): String = {
    s"EventEnvelope[$uid]($event, $sender)"
  }
}


//===========================================================================
//========================== Stream part ===================================
//===========================================================================
case class Subscribe(actor: ActorRef)

case class Unsubscribe(actor: ActorRef)

class EventStreamActor extends Actor with ActorLogging {

  def loop(subscribers: List[ActorRef]): Receive = {
    case Subscribe(subscriber) =>
      log.debug(s"Subscribe from: $subscriber")
      context.watch(subscriber)
      context.become(loop(subscriber :: subscribers))
    case Terminated(subscriber) =>
      log.debug(s"Terminated from: $subscriber")
      context.become(loop(subscribers.filterNot(_ == subscriber)))
    case Unsubscribe(subscriber) =>
      log.debug(s"Unsubscribe from: $subscriber")
      context.become(loop(subscribers.filterNot(_ == subscriber)))
    case event: EventEnvelope =>
      log.debug(s"Received: $event")
      subscribers.foreach(_ ! event)
  }

  override def receive: Actor.Receive = loop(Nil)
}

trait Behavior[S] extends Actor with ActorLogging with Stash {

  private var _state: S = _
  private var _stateChangeIsAllowed = false

  def state: S = _state

  def state_=(newState: S): Unit = {
    if (_stateChangeIsAllowed) {
      _state = newState
    } else {
      sys.error("State change is not allowed now")
    }
  }

  type EventReceive = BehaviorContext.EventReceive

  private[this] var _executionContext: ExecutionContext = _
  implicit lazy val ec: ExecutionContext = _executionContext

  case class OnSuccess[T](f: T => List[Event], result: T)

  def onSuccess[T](future: => T)(f: T => List[Event])(implicit ec: ExecutionContext): Unit = {
    Future(future).onComplete {
      case Success(r) => self ! OnSuccess(f, r)
      case Failure(error) =>
    }
  }


  def receive: Receive = {
    case InitBehaviorContext(f) =>
      val behaviorContext = f(context, self)
      behaviorContext.eventStream ! Subscribe(self)
      _executionContext = behaviorContext.executionContext

      unstashAll()
      context.become(contextInitialized(behaviorContext))
      self ! Init
    case _ =>
      stash()
  }

  def contextInitialized(behaviorContext: BehaviorContext): Receive = {
    case OnSuccess(f, result) =>
      _stateChangeIsAllowed = true
      val events = f(result).map(EventEnvelope(self, _))
      _stateChangeIsAllowed = false
      events.foreach(behaviorContext.eventStream ! _)
    case c: Command if onCommand.isDefinedAt(c) =>
      _stateChangeIsAllowed = true
      val events = onCommand(c).map(EventEnvelope(self, _))
      _stateChangeIsAllowed = false
      events.foreach(behaviorContext.eventStream ! _)
    case EventEnvelope(sender, event) if sender == self && onEvent.isDefinedAt(event) =>
      onEvent(event)

    case e: EventEnvelope if onEvent.isDefinedAt(e) =>
      onEvent(e)

    case c: Command =>
    case e: EventEnvelope =>

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand: PartialFunction[Command, List[Event]] = new PartialFunction[Command, List[Event]] {
    override def isDefinedAt(x: Command): Boolean = false

    override def apply(v1: Command): List[Event] = throw new IllegalAccessError("Not defined")
  }

  def onEvent = new PartialFunction[Event, Command] {
    override def isDefinedAt(x: Event): Boolean = false

    override def apply(v1: Event): Command = throw new IllegalAccessError("Not defined")
  }
}

object BehaviorContext {
  type EventReceive = PartialFunction[Event, Unit]

  case class InitBehaviorContext(f: (ActorRefFactory, ActorRef) => BehaviorContext)

}

class BehaviorContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, val executionContext: ExecutionContext) {
  def behaviorOf(creator: Props): ActorRef = {
    val behavior = actorRefFactory.actorOf(creator, s"Behavior-${UUID.randomUUID().toString}")
    behavior ! InitBehaviorContext(new BehaviorContext(_, _, eventStream, executionContext))
    behavior
  }

}

case class ProgressBar(on: Boolean) extends Event

case class CounterChanged(counter: Int) extends Event

/**
  * SimpleUI Behavior
  */

case class State(counter: Int, progressCounter: Int)

class SimpleUIBehavior extends Behavior[State] {

  case object ProgressOn extends Command
  case object ProgressOff extends Command

  override def onCommand: PartialFunction[Command, List[Event]] = {
    case Init =>
      state = State(0, 0)
      List(Initialized)
    case ProgressOn =>
      val counter = state.progressCounter
      if(state.progressCounter == 0) println("StartProgress")
      state = state.copy(progressCounter = counter + 1)
      Nil
    case ProgressOff =>
      val counter = state.progressCounter
      state = state.copy(progressCounter = counter - 1)
      if(state.progressCounter == 0) println("StopProgress")
      Nil
    case IncCounter =>
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { result =>
        state = state.copy(counter = state.counter + result)
        self ! ProgressOff
        List(CounterChanged(state.counter))
      }
      self ! ProgressOn
      Nil
    case DecCounter => //Decrement counter by service
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { result =>
        state = state.copy(counter = state.counter - result)
        self ! ProgressOff
        List(ProgressBar(false), CounterChanged(state.counter))
      }

      state = state.copy(progressCounter = state.progressCounter + 1)
      self ! ProgressOn
      Nil
  }

  //    override def onEvent = {
  //      case Initialized => Nil
  //    }

}


//===========================================================================
//======================== Controller part ==================================
//===========================================================================

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

case object IncCounter extends Command

case object DecCounter extends Command


class SimpleUIView extends View {
  @FXML private var _incButton: Button = _

  def incButton = _incButton

  @FXML private var _decButton: Button = _

  def decButton = _decButton

  @FXML private var _counter: Label = _

  def counter = _counter

}

class SimpleUIController extends Controller[SimpleUIView] {

  def onEvent: EventReceive = {
    case Initialized => init()
    case ProgressBar(on) =>
      ui { view =>
        println(s"ProgressBar: $on")
      }
    case CounterChanged(counter) => ui { view =>
      view.counter.setText(s"$counter")
    }
  }


  def init(): Ui = ui { view =>
    //this runs on ui thread
    view.incButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        println("Sending Inc")
        behavior ! IncCounter
      }
    })

    view.decButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        println("Sending Dec")
        behavior ! DecCounter
      }
    })
  }

}

//===========================================================================
//======================== Application part =================================
//===========================================================================
object JavaFXExecutor extends Executor {
  new JFXPanel

  override def execute(command: Runnable): Unit = Platform.runLater(command)
}

object SimpleUI extends App {

  val system = ActorSystem("UI-system")

  val eventStream = system.actorOf(Props(new EventStreamActor), s"EventStreamActor")

  val backgroundExecutor = scala.concurrent.ExecutionContext.global
  val sideEffectExecutor = ExecutionContext.fromExecutor(JavaFXExecutor)

  system.actorOf(Props(new Actor {
    val behaviorContext = new BehaviorContext(context, self, eventStream, backgroundExecutor)

    val controllerContext = new ControllerContext(system, self, eventStream, backgroundExecutor, behaviorContext)
    val parent = controllerContext.load[SimpleUIController]

    Platform.runLater(new Runnable {
      override def run(): Unit = {
        val stage = new Stage
        stage.setScene(new Scene(parent))
        stage.setTitle("CounterPair App")
        stage.show()
      }
    })


    //    behaviorRef ! Init

    override def receive: Actor.Receive = {
      case e => println(s"$e received")
    }
  }), s"MainActor")

}



