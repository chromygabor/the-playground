package com.chromy.reactiveui.myjavafx

import javafx.beans.property.StringProperty
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.Utils._
import com.chromy.reactiveui.{Signal}

/**
 * Created by chrogab on 2015.04.28..
 */

case object Increment extends Counter.Actions({model =>
  model.copy(counter = model.counter + 1)
})
case object Decrement extends Counter.Actions({model => model.copy(counter = model.counter - 1)})
case object Clear extends Counter.Actions({model => CounterModel()})

case class CounterModel(counter: Int = 0)

object Counter extends Module[CounterModel, Counter] {
  def clear(model: Model) : Model = model.copy(counter = 0)

  def render(model: Model, controller: Controller, address: Signal.Address[Counter.Actions]): Unit = {
    controller.counterLabel.setText(model.counter.toString)

    controller.incrementButton.setOnAction { () => address.onNext(Increment) }
    controller.decrementButton.setOnAction { () => address.onNext(Decrement) }
  }
}

// This is for future solutions
//class Render(model: Signal.Signal[CounterModel], address: Signal.Address[Counter.Actions]) extends Counter {
//  model.map {_.counter.toString} subscribe ( {value => counterLabel.textProperty.setValue(value)})
//
//  incrementButton.setOnAction { () => address.onNext(Increment) }
//  decrementButton.setOnAction { () => address.onNext(Decrement) }
//}

class Counter extends FlowPane {
  @FXML private var _counterLabel:Label = _
  @FXML private var _incrementButton: Button = _
  @FXML private var _decrementButton: Button = _

  def counterLabel = _counterLabel
  def incrementButton = _incrementButton
  def decrementButton = _decrementButton
}