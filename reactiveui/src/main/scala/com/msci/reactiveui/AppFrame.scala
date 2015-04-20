package com.msci.reactiveui

import javafx.fxml.FXML
import javafx.scene.control.Label
import javafx.scene.layout.AnchorPane

/**
 * Created by cry on 2015.04.17..
 */
class AppFrame extends AnchorPane with Component[AppFrameModel] {

  @FXML
  private[this] var _buttons: Buttons = _

  @FXML
  private[this] var _label: Label = _


  override def update(action: Action, model: AppFrameModel): AppFrameModel = action match {
    case Action(Buttons.Increment, id) =>
      model.copy(counter = model.counter + 1, buttons = _buttons.update(action, model.buttons))
    case Action(Buttons.Decrement, id) =>
      model.copy(counter = model.counter - 1, buttons = _buttons.update(action, model.buttons))
    case Action(action, _) =>
      model.copy(buttons = _buttons.update(action, model.buttons))
  }

  override def render(model: AppFrameModel): Unit = {
    _label.setText("counter: " + model.counter)
    _buttons.render(model.buttons)
  }
}

case class AppFrameModel(counter: Int=0, buttons: Buttons.Model = Buttons.Model)
