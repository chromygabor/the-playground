package com.chromy.reactiveui.myjavafx

import javafx.collections.ListChangeListener
import javafx.collections.ListChangeListener.Change
import javafx.fxml.FXML
import javafx.scene.Node
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.HBox

import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.{Dispatcher, Atom}
import com.chromy.reactiveui.myjavafx.Counter.{Decrement, Increment}
import monocle.macros.GenLens
import rx.lang.scala.{Subscriber, Observable, Observer}

case class CounterPairModel(leftCounter: CounterModel = CounterModel(),
                            rightCounter: CounterModel = CounterModel())


case class CounterPairDispatcher(parent: Dispatcher[CounterPairModel, Action], actions: Observer[Action], changes: Observable[CounterPairModel], subscriber: Subscriber[CounterPairModel]) {
  val leftCounter = GenLens[CounterPairModel](_.leftCounter)
  val rightCounter = GenLens[CounterPairModel](_.rightCounter)

  changes.distinctUntilChanged.subscribe(subscriber)
}

object CounterPair {
  def upd(actions: Observer[Action])(model: CounterPairModel, action: Action): CounterPairModel = model
  def init() = ???
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

  def render(actions: Observer[Action])(model : CounterPairModel): Unit = {
    leftLabel.setText(model.leftCounter.value.toString)
    rightLabel.setText(model.rightCounter.value.toString)
  }
  def error(err: Throwable): Unit = {
  }
  def complete(): Unit = {
  }

  def dispatch(parentFactory: DispatcherFactory[CounterPairModel, Action], actions: Observer[Action], changes: Observable[CounterPairModel]) = {
    val parent = parentFactory.subscribe(CounterPair.upd(actions))
    val dispatcher = CounterPairDispatcher(parent, actions, changes, Subscriber({ model => render(actions)(model)}, { err => error(err)}, { () => complete()}))

    leftCounterController.dispatch(parent.fromLens(dispatcher.leftCounter), actions, changes.map { dispatcher.leftCounter.get })
    rightCounterController.dispatch(parent.fromLens(dispatcher.rightCounter), actions, changes.map { dispatcher.rightCounter.get })
  }

}