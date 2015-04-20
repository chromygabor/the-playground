package com.chromy.reactiveui

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.Button
import javafx.scene.layout.AnchorPane
import Buttons.{Decrement, Increment}
import Utils._

/**
 * Created by cry on 2015.04.17..
 */
object Buttons {
  type Model = ButtonsModel
  lazy val Model = ButtonsModel()

  case object Increment extends Action
  case object Decrement extends Action
}
class Buttons extends AnchorPane with Component[ButtonsModel] {
  @FXML
  private[this] var _increment: Button = _

  @FXML
  private[this] var _decrement: Button = _

  _increment.setOnAction { () => send(Increment) }
  _decrement.setOnAction { () => send(Decrement) }

  override def update(action: Action, model: ButtonsModel): ButtonsModel = action match {
    case Action(Increment, id) => model.copy(actualValue = model.actualValue + 1)
    case Action(Decrement,id) => model.copy(actualValue = model.actualValue - 1)
    case Action(_,id) => model
  }

  override def render(model: ButtonsModel): Unit = {
    _increment.setText(model.incrementText)
    _decrement.setText(model.decrementText)
  }
}

case class ButtonsModel(actualValue:Int = 0) {
  val incrementText: String = actualValue + " +1"
  val decrementText: String = actualValue + " -1"
}
