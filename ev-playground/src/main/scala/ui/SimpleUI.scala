package ui

import java.net.URL
import java.util.concurrent.Executor
import java.util.{ResourceBundle, UUID}
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.util.Callback

import akka.actor._
import ui.BehaviorContext.InitBehaviorContext
import ui.ControllerContext.InitControllerContext

import scala.concurrent.{ExecutionContext, Future}

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

trait Behavior extends Actor with ActorLogging with Stash {

  type EventReceive = BehaviorContext.EventReceive

  private[this] var _executionContext: ExecutionContext = _
  implicit lazy val ec: ExecutionContext = _executionContext

  def receive: Receive = {
    case InitBehaviorContext(f) =>
      val behaviorContext = f(context, self)
      behaviorContext.eventStream ! Subscribe(self)

      _executionContext = behaviorContext.executionContext

      unstashAll()
      context.become(contextInitialized(behaviorContext))
    case _ =>
      stash()
  }

  def contextInitialized(behaviorContext: BehaviorContext): Receive = {
    case c: Command if onCommand(behaviorContext.commandContext).isDefinedAt(c) =>
      val r = onCommand(behaviorContext.commandContext)(c)
      behaviorContext.commandContext.fire(r)

    case EventEnvelope(sender, event) if sender == self && onEvent.isDefinedAt(event) =>
      onEvent(event)

    case e: EventEnvelope if onEvent.isDefinedAt(e) =>
      onEvent(e)

    case c: Command =>
    case e: EventEnvelope =>

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand(context: CommandContext): PartialFunction[Command, List[Event]] = new PartialFunction[Command, List[Event]] {
    override def isDefinedAt(x: Command): Boolean = false

    override def apply(v1: Command): List[Event] = throw new IllegalAccessError("Not defined")
  }

  def onEvent: EventReceive = new PartialFunction[Event, Unit] {
    override def isDefinedAt(x: Event): Boolean = false

    override def apply(v1: Event): Unit = throw new IllegalAccessError("Not defined")
  }
}

object BehaviorContext {
  type EventReceive = PartialFunction[Event, Unit]

  case class InitBehaviorContext(f: (ActorRefFactory, ActorRef) => BehaviorContext)

}

case class CommandContext(owner: ActorRef, eventStream: ActorRef) {
  def fire(event: Event): Unit = eventStream.tell(EventEnvelope(owner, event), owner)

  def fire(events: Seq[Event]): Unit = events.foreach(event => eventStream.tell(EventEnvelope(owner, event), owner))
}

class BehaviorContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, val executionContext: ExecutionContext) {
  def behaviorOf(creator: Props): ActorRef = {
    val behavior = actorRefFactory.actorOf(creator, s"Behavior-${UUID.randomUUID().toString}")
    behavior ! InitBehaviorContext(new BehaviorContext(_, _, eventStream, executionContext))
    behavior
  }

  lazy val commandContext = new CommandContext(owner, eventStream)
}

case class ProgressBar(on: Boolean) extends Event
case class CounterChanged(counter: Int) extends Event

class SimpleUIBehavior extends Behavior {

  case class StateChange(f: State => (State, List[Event])) extends Command

  case class State(counter: Int, progressBarVisible: Boolean)

  private var _state = State(0, false)
  // def state = _state
  // def state_=(f: State => State) = _state = newState

  def change(f: State => State)(f2: State => List[Event]): Unit = {
    self ! StateChange { state =>
      val newState = f(state)
      newState -> f2(newState)
    }
  }


  override def onCommand(context: CommandContext): PartialFunction[Command, List[Event]] = {
    case StateChange(f) => {
      val (newState, events) = f(_state)
      _state = newState
      events
    }

    case Init =>
      List(Initialized)
    case IncCounter =>
      Future {
        //This is a service call
        Thread.sleep(1000)
        1
      }.foreach { result =>
        change( state => state.copy(counter = state.counter + result) ) { newState =>
          List(ProgressBar(false), CounterChanged(newState.counter))
        }
      }
      List(ProgressBar(true))
    case DecCounter =>    //Decrement counter by service
      Future {
        //This is a service call
        Thread.sleep(1000)
        1
      }.foreach { result =>
        change( state => state.copy(counter = state.counter - result) ) { newState =>
          List(ProgressBar(false), CounterChanged(newState.counter))
        }

      }
      List(ProgressBar(true))
  }

  //  def eventListener: Event => Command = {
  //
  //  }
  //
  //  override def onEvent: EventReceive = {
  //    case Initialized =>
  //      println("Initialized event received in behavior")
  //    case CounterIncremented =>
  //  }

}


//===========================================================================
//======================== Controller part ==================================
//===========================================================================

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

case object IncCounter extends Command

case object DecCounter extends Command

class SimpleUIView {
  @FXML private var _incButton: Button = _

  def incButton = _incButton

  @FXML private var _decButton: Button = _

  def decButton = _decButton

  @FXML private var _counters: FlowPane = _

  def counters = _counters
}

class SimpleUIController(behavior: ActorRef) extends Controller[SimpleUIView] {

  def r: EventReceive = {
    case Initialized => init()
    case ProgressBar(on) =>
      ui { view =>
        println(s"ProgressBar: $on")
      }
    case CounterChanged(counter) => ui{ _ =>
      println(s"Counter changer: $counter")
    }
  }


  def init(): Ui = ui { view =>
    //this runs on ui thread
    view.incButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        behavior ! IncCounter
      }
    })

    view.decButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        behavior ! DecCounter
      }
    })
  }

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    println(behavior)
  }


}

//===========================================================================
//======================== Application part =================================
//===========================================================================
object JavaFXExecutor extends Executor {
  println("Executor created")
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
    val (controller, parent) = controllerContext.load[SimpleUIController]

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



