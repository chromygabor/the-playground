package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.HBox

import com.chromy.reactiveui.Signal
import com.chromy.reactiveui.Utils._

import scala.concurrent.{Promise, Future}
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{Failure, Success}

/**
 * Created by chrogab on 2015.04.28..
 */

case class Left(action: ModuleAction[Counter.Model]) extends CounterPair.Actions({ model =>
//  val f = for {
//      leftValue <- Counter.update(action, model.leftValue)
//  } yield {
//      println(s"Left on thread ${Thread.currentThread.getName}")
//      model.copy(leftValue = leftValue)
//    }
//  f

  val p = Promise[CounterPair.Model]

  val f = Counter.update(action, model.leftValue).map {leftValue =>  model.copy(leftValue = leftValue)}
  f.onComplete {
    case Success(value) => p.success(value)
    case Failure(error) =>
  }

  p.future
})

case class Right(action: ModuleAction[Counter.Model]) extends CounterPair.Action.Async({model =>
  Counter.update(action, model.rightValue).map { in =>
    println(s"Right on thread ${Thread.currentThread.getName}")
    model.copy(rightValue = in)
  }
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

  @FXML private var _leftCounterController: Counter = _
  @FXML private var _rightCounterController: Counter = _

  def leftClear = _leftClear

  def rightClear = _rightClear

  def leftLabel = _leftLabel

  def rightLabel = _rightLabel

  def leftCounterController = _leftCounterController

  def rightCounterController = _rightCounterController
}