package ui

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
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



