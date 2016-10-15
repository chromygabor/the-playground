package ui

import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.ObservableStringValue
import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control.{Label, Button}

/**
  * Created by chrogab on 2016.10.12..
  */
case class CounterState()

case object Increment extends Command
case object Decrement extends Command

class CounterBehavior extends Behavior[CounterState] {

  override def initialState: CounterState = CounterState()

  override def onCommand(state: CounterState): PartialFunction[Command, List[Event]] = {
    case Increment =>
      Nil
    case Decrement =>
      Nil
    case e =>
      log.info(s"Event received: $e")
      Nil
  }

  override def onEvent(state: CounterState, context: Context): PartialFunction[Event, CounterState] = {
    case e => state
  }
}


class CounterController extends Controller {

//  @FXML private var _btnDecrement: Button = _
//  lazy val btnDecrement = _btnDecrement
  @FXML private var _lblCounter: Label = _
  lazy val lblCounter = _lblCounter.textProperty()
//  @FXML private var _btnIncrement: Button = _
//  lazy val btnIncrement = _btnIncrement
//  @FXML private var _btnClose: Button = _
//  lazy val btnClose = _btnClose

  def incrementClicked(event: ActionEvent): Unit =  behavior ! Increment
  def decrementClicked(event: ActionEvent): Unit =  behavior ! Increment
  def closeClicked(event: ActionEvent): Unit =  behavior ! Increment

  override def onEvent: EventReceive = {
    case Initialized =>
      ui {
        lblCounter.set("0")
      }
  }
}
