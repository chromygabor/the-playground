package ui

import javafx.application.Platform
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.Scene
import javafx.scene.control.{Button, Label}
import javafx.stage.Stage

import akka.actor._

import scala.concurrent.ExecutionContext

/**
  * Created by chrogab on 2016.09.13..
  */

case object Init extends Command

case object Initialized extends Event

case object DummyEvent extends Event

case object Empty extends Command

/**
  * SimpleUI Behavior
  */

case class State(counter: Int, progressBar: Boolean)
case class CounterChanged(counter: Int) extends Event
case class ProgressBar(on: Boolean) extends Event

class SimpleUIBehavior extends Behavior[State] {

  implicit def commandToList(cmd: Command): List[Event] = ???

  case object ProgressBarOn extends Command
  case object ProgressBarOff extends Command

  var progressCounter = 0

  override def onCommand(state: State): PartialFunction[Command, List[Event]] = {
    case Init =>
      List(Initialized)
    case ProgressBarOn =>
      progressCounter = progressCounter + 1
      if(progressCounter == 1) List(ProgressBar(true))
      else Nil
    case ProgressBarOff =>
      progressCounter = progressCounter - 1
      if(progressCounter == 0) List(ProgressBar(false))
      else Nil

    case IncCounter =>
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { (state, result) =>
        CounterChanged(state.counter) :: ProgressBarOff
      }
      ProgressBarOn
    case DecCounter => //Decrement counter by service
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { (state, result) =>
        CounterChanged(state.counter) :: ProgressBarOff
      }

      ProgressBarOn
  }

  def oe(state: State): PartialFunction[Event, State] = {
    case CounterChanged(counter) => state.copy(counter = counter)
    case ProgressBar(on) => state.copy(progressBar = on)
    case e =>
      state
  }

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

  def onEvent(): EventReceive = {
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



