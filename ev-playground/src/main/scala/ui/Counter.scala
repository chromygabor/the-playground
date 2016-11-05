package ui

import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control.Label

import akka.actor.ActorRef
import ui.CounterService.{DecrementCounter, IncrementCounter}
import ui.MyTask.{TaskFinished, TaskStarted}

import scala.concurrent.Future
import akka.pattern._

/**
  * Created by chrogab on 2016.10.12..
  */
case class CounterState()

case object Increment extends Command
case object Decrement extends Command
case object Close extends Command

case class CounterChanged(value: Int) extends PersistentEvent


object CounterBehavior {
  val CounterTaskId = SimpleTaskId()
}

class CounterBehavior(initialValue: Int, counterService: ActorRef) extends Behavior[CounterState] {

  override def initialState: CounterState = CounterState()

  override def onCommand(state: CounterState): PartialFunction[Command, Seq[Event]] = {
    case Init =>
      CounterChanged(initialValue)
    case Increment =>
      taskAs[Int](CounterBehavior.CounterTaskId, counterService ? IncrementCounter(behaviorId)).onSuccess { (state, value) =>
        CounterChanged(value)
      }.onFailure { (state, error ) =>
        log.error("Error occurred during Increment: ", error)
        error.printStackTrace()
        Nil
      }
    case Decrement =>
      taskAs[Int](CounterBehavior.CounterTaskId, counterService ? DecrementCounter(behaviorId)).onSuccess { (state, value) =>
        CounterChanged(value)
      }.onFailure { (state, error ) =>
        log.error("Error occurred during Decrement: ", error)
        error.printStackTrace()
        Nil
      }
    case Close =>
      Nil
    case e =>
      log.info(s"Command received in CounterBehavior: $e")
      Nil
  }

  override def onEvent(state: CounterState, context: BehaviorContext): PartialFunction[Event, CounterState] = {
    case e =>
      log.debug(s"Received: $e")
      state
  }
}

class CounterController extends Controller {

  @FXML private var _lblCounter: Label = _
  lazy val lblCounter = _lblCounter.textProperty()

  def incrementClicked(event: ActionEvent): Unit =  behavior ! Increment
  def decrementClicked(event: ActionEvent): Unit =  behavior ! Decrement
  def closeClicked(event: ActionEvent): Unit =  behavior ! Close

  override def onEvent: EventReceive = {
    case TaskStarted(CounterBehavior.CounterTaskId) => {
      log.debug("TaskStarted: Counter")
      ui { lblCounter.set("...") }
    }
    case TaskFinished(CounterBehavior.CounterTaskId) => {
      log.debug("TaskFinished: Counter")
      ui()
    }

    case Initialized =>
      ui {
        lblCounter.set("0")
      }
    case CounterChanged(value) =>
      log.debug(s"CounterChanged: $value")
      ui {
        lblCounter.set(value.toString)
      }
  }
}
