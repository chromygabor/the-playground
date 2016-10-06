package ui

import javafx.application.Platform
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.Scene
import javafx.scene.control.{Button, Label}
import javafx.stage.Stage

import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext

/**
  * Created by chrogab on 2016.09.13..
  */

case object Init extends Command

case object Initialized extends PersistentEvent

case object Empty extends Command

/**
  * SimpleUI Behavior
  */

case class CounterChanged(counter: Int) extends PersistentEvent
case class ProgressBar(on: Boolean) extends SimpleEvent

case object IncCounter extends Command
case object DecCounter extends Command

case class State(counter: Int)
class SimpleUIBehavior(val id: String) extends Behavior[State] {

  case object IncProgressCounter extends Command
  case object DecProgressCounter extends Command

  var progressCounter = 0

  val initialState = State(0)

  override def onCommand(state: State): PartialFunction[Command, List[Event]] = {
    case Init =>
      println("Init command received in behavior")
      List(Initialized)
    case IncProgressCounter =>
      progressCounter = progressCounter + 1
      if(progressCounter == 1) ProgressBar(true)
      else Nil
    case DecProgressCounter =>
      progressCounter = progressCounter - 1
      if(progressCounter == 0) ProgressBar(false)
      else Nil

    case IncCounter =>
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { (state, result) =>
        CounterChanged(state.counter + result) :: DecProgressCounter
      }
      IncProgressCounter
    case DecCounter => //Decrement counter by service
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { (state, result) =>
        CounterChanged(state.counter - result) :: DecProgressCounter
      }

      IncProgressCounter
  }

  override def onEvent(state: State): PartialFunction[Event, State] = {
    case CounterChanged(counter) => state.copy(counter = counter)
    case Initialized =>
      println("Initialized event received in behavior")
      state
  }

}


class SimpleUIController extends Controller {
  @FXML private var _incButton: Button = _

  def incButton = _incButton

  @FXML private var _decButton: Button = _

  def decButton = _decButton

  @FXML private var _counter: Label = _

  def counter = _counter

  def onEvent: EventReceive = {
    case Initialized => init()
    case ProgressBar(on) => ui {
        println(s"ProgressBar: $on")
    }
    case CounterChanged(newCounter) => ui {
      counter.setText(s"$newCounter")
    }
  }

  def init(): Ui = ui {
    //this runs on ui thread
    incButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        behavior ! IncCounter
      }
    })

    decButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        behavior ! DecCounter
      }
    })
  }

}

class AppBehavior extends Behavior[Int] {
  val backgroundExecutor = scala.concurrent.ExecutionContext.global

  val eventStream = context.actorOf(Props(new EventStreamActor), s"EventStreamActor")

  val behaviorContext = new BehaviorContext(context, self, eventStream, backgroundExecutor, Timeout(5.seconds))

  override def initialState: Int = 0

  override def id: String = "AppBehavior"

  override def onCommand(state: Int): PartialFunction[Command, List[Event]] = {
    case Init =>
      println("Init command in behavior")
      Nil
    case Subscribed =>
      List(Initialized)
  }

}

class AppController extends Controller {

//  val controllerContext = new ControllerContext(system, self, eventStream, backgroundExecutor, behaviorContext)


  override def onEvent: EventReceive = {
    case Initialized => ui{
      println(s"App Controller initialized")
    }
    case e => ui {
      println(s"App Controller event: $e")
    }
  }
}

object SimpleUI extends App {

  val system = ActorSystem("UI-system")

  //val controllerActor = system.actorOf(Props(new ))

  val eventStream = system.actorOf(Props(new EventStreamActor), s"EventStreamActor")

  val backgroundExecutor = scala.concurrent.ExecutionContext.global
  val sideEffectExecutor = ExecutionContext.fromExecutor(JavaFXExecutor)

  system.actorOf(Props(new Actor {
    val behaviorContext = new BehaviorContext(context, self, eventStream, backgroundExecutor, Timeout(5.seconds))
    val controllerContext = new ControllerContext(system, self, eventStream, backgroundExecutor, behaviorContext)

//    val (ctrl, behavior) = controllerContext.controllerOf[AppController]

    val behavior = behaviorContext.behaviorOf(Props(new SimpleUIBehavior("SimpleUIBehavior")))
    val parent = controllerContext.load[SimpleUIController](behavior)
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



