package ui

import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import akka.actor.Props

/**
  * Created by chrogab on 2016.10.12..
  */
case class CountersState()

case object AddCounter extends Command

case class CounterAdded(behavior: BehaviorId = BehaviorId[CounterBehavior]) extends PersistentEvent

class CountersBehavior extends Behavior[CountersState] {
  override def initialState: CountersState = CountersState()

  override def onCommand(state: CountersState): PartialFunction[Command, List[Event]] = {
    case AddCounter =>
      CounterService.behaviorId ? AddCounter
      Nil

    case Init =>
      Initialized
  }

  override def onEvent(state: CountersState, context: Context): PartialFunction[Event, CountersState] = {
    case Initialized =>
      state

    case CounterAdded(behaviorId) =>
      context.create(behaviorId, new CounterBehavior)
      state
  }
}

class CountersController extends Controller {

  @FXML private var _bAdd: Button = _

  def bAdd = _bAdd

  @FXML private var _pCounters: FlowPane = _

  def pCounters = _pCounters

  def addCounter(event: ActionEvent): Unit =  behavior ! AddCounter

  override def onEvent: EventReceive = {
    case CounterAdded(behaviorId) =>
      ui {
        pCounters.getChildren.add(load[CounterController](behaviorId))
      }
    case e =>
      ui {
        log.info(s"Event received: $e")
      }
  }
}
