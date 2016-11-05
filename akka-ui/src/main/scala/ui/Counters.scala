package ui

import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import akka.actor.{ActorRef, Props}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Created by chrogab on 2016.10.12..
  */
case class CountersState()

case object AddCounter extends Command

case class CounterAdded(behavior: BehaviorId, initialValue: Int) extends PersistentEvent

object CountersBehavior {
  val CountersTaskId = QueueTaskId()
}

/**
  * Behavior
  * @param counterService
  */
class CountersBehavior(counterService: ActorRef) extends Behavior[CountersState] {

  import akka.pattern._

  override def initialState: CountersState = CountersState()

  override def onCommand(state: CountersState): PartialFunction[Command, List[Event]] = {
    case AddCounter =>
      log.debug("AddCounter")
      taskAs[(BehaviorId, Int)](CountersBehavior.CountersTaskId, counterService ? AddCounter)
        .onSuccess { case (s, (behaviorId, initialValue)) =>
          CounterAdded(behaviorId, initialValue)
        }.onFailure { case (s, error) =>
          Nil
        }
        Nil
    case Init =>
      Initialized
  }

  override def onEvent(state: CountersState, context: BehaviorContext): PartialFunction[Event, CountersState] = {
    case Initialized =>
      state

    case CounterAdded(behaviorId, initialValue) =>
      context.create(behaviorId, Props(new CounterBehavior(initialValue, counterService)))
      state
  }
}

/**
  * Controller
  */
class CountersController extends Controller {

  @FXML private var _bAdd: Button = _

  def bAdd = _bAdd

  @FXML private var _pCounters: FlowPane = _

  def pCounters = _pCounters

  def addCounter(event: ActionEvent): Unit = behavior ! AddCounter

  override def onEvent: EventReceive = {
    case CounterAdded(behaviorId, initialValue) =>
      ui {
        pCounters.getChildren.add(load[CounterController](behaviorId))
      }
    case e =>
      ui {
        log.info(s"Event received: $e")
      }
  }
}
