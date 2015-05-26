package com.chromy.reactiveui.myjavafx

import javafx.collections.ListChangeListener
import javafx.collections.ListChangeListener.Change
import javafx.fxml.FXML
import javafx.scene.Node
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.HBox

import com.chromy.reactiveui.Atom

//package com.chromy.reactiveui.myjavafx
//
//import javafx.fxml.FXML
//import javafx.scene.control.{Button, Label}
//import javafx.scene.layout.HBox
//
//import com.chromy.reactiveui.Signal
//import com.chromy.reactiveui.Signal.Mailbox
//import com.chromy.reactiveui.Utils._
//
///**
// * Created by chrogab on 2015.04.28..
// */
//
//trait WrapperAction extends Action {
//  val action: Action
//  val originalAction: Action = action match {
//    case e: WrapperAction => e.originalAction
//    case _ => action
//  }
//  def apply(action: Action): WrapperAction
//}
//
//case class Left(action: Action) extends WrapperAction
//case class Right(action: Action) extends WrapperAction
//
//
//case class CounterPairModel(leftValue: Counter.Model = new Counter.Model,
//                            rightValue: Counter.Model = new Counter.Model)
//
//object CounterPair extends Module[CounterPairModel, CounterPair] {
//  def render(model: Model, controller: Controller, address: Signal.Address[Action]): Unit = {
//    controller.leftLabel.setText(model.leftValue.counter.toString)
//    controller.rightLabel.setText(model.rightValue.counter.toString)
//
//    controller.leftClear.setOnAction { () => address.onNext(Left(Clear)) }
//    controller.rightClear.setOnAction { () => address.onNext(Right(Clear)) }
//
//    Counter.render(model.leftValue, controller.leftCounterController)
//    Counter.render(model.rightValue, controller.rightCounterController)
//  }
//
//  override def update: (Action, CounterPairModel) => CounterPairModel = { (action, model) =>
////    (action match {
////      case _ => model
////    }).dispatch(action)
//    ???
//  }
//}
//

case class CounterPairModel(leftCounter: CounterModel = CounterModel(),
                            rightCounter: CounterModel = CounterModel())

case class CounterPairDispatch(parent : Atom[CounterPairModel]) {
  val leftCounter = parent.map(_.leftCounter)(newChild => _.copy(leftCounter = newChild))
  val rightCounter = parent.map(_.rightCounter)(newChild => _.copy(rightCounter = newChild))
}

object CounterPair extends Module[CounterPair] {
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

  def dispatch(parent: Atom[CounterPairModel]) = {
    val dispatcher = CounterPairDispatch(parent)
    leftCounterController.dispatch(dispatcher.leftCounter)
    rightCounterController.dispatch(dispatcher.rightCounter)
  }
}