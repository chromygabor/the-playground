package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.HBox

import com.chromy.reactiveui.Signal
import com.chromy.reactiveui.Utils._

/**
 * Created by chrogab on 2015.04.28..
 */

case class Left(action: ModuleAction[Counter.Model]) extends CounterPair.Actions({ model =>
  model.copy(leftValue = Counter.update(action, model.leftValue))
})

case class Right(action: ModuleAction[Counter.Model]) extends CounterPair.Actions ({  model =>
  model.copy(rightValue = Counter.update(action, model.rightValue))
})


case class CounterPairModel(leftValue: Counter.Model = new Counter.Model, rightValue: Counter.Model = new Counter.Model)

object CounterPair extends Module[CounterPairModel, CounterPair] {
  def render(model: Model, controller: Controller, address: Signal.Address[CounterPair.Actions]): Unit = {
    controller.leftLabel.setText(model.leftValue.counter.toString)
    controller.rightLabel.setText(model.rightValue.counter.toString)

    controller.leftClear.setOnAction { () => address.onNext(Left(Clear)) }
    controller.rightClear.setOnAction { () => address.onNext(Right(Clear)) }

    Counter.render(model.leftValue, controller.leftCounterController, Signal.forwardTo(address) { action: ModuleAction[Counter.Model] => Left(action) })
    Counter.render(model.rightValue, controller.rightCounterController, Signal.forwardTo(address) { action: ModuleAction[Counter.Model] => Right(action) })
  }

}

class CounterPair extends HBox {
  @FXML private var _leftClear: Button = _
  @FXML private var _rightClear: Button = _

  @FXML private var _leftLabel: Label = _
  @FXML private var _rightLabel: Label = _

  @FXML private var _leftCounterController: Counter  = _
  @FXML private var _rightCounterController: Counter  = _

  def leftClear = _leftClear
  def rightClear = _rightClear

  def leftLabel = _leftLabel
  def rightLabel = _rightLabel

  def leftCounterController = _leftCounterController
  def rightCounterController = _rightCounterController
}