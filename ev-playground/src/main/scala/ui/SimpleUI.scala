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



