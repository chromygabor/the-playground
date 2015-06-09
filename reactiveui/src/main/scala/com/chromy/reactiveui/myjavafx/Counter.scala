package com.chromy.reactiveui.myjavafx

import java.util.UUID
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.{Dispatcher, Atom}
import com.chromy.reactiveui.Utils._
import com.chromy.reactiveui.myjavafx.Counter.{Decrement, Increment}
import rx.lang.scala.{Observable, Subject, Observer}

import scala.concurrent.Future
import scala.util.{Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

case class CounterModel(value: Int = 0){
  val id: String = UUID.randomUUID().toString
}

case class CounterD(parent: Dispatcher[CounterModel, Action], actions: Observer[Action]) {

}

object Counter {
  type Model = CounterModel

  case class Increment(id: String) extends LocalAction
  case class Decrement(id: String) extends LocalAction
  case class StateChanged(id: String) extends Action

  //LocalAction?

  def upd(action: Action, model: CounterModel): CounterModel = action match {
    case Increment(model.id) =>
      //println(s"update: $id")
      model.copy(value = model.value + 1)
    case Decrement(model.id) =>
      //println(s"update: $id")
      val newValue = model.value - 1
      if (newValue == 0) {
        Future {
          println("We are waiting")
          Thread.sleep(5000)
          println("We are ready")
        }
      }
      model.copy(value = newValue)
    case _ => model
  }
}


class Counter extends FlowPane {
  @FXML private var _counterLabel: Label = _
  @FXML private var _incrementButton: Button = _
  @FXML private var _decrementButton: Button = _

  def counterLabel = _counterLabel

  def incrementButton = _incrementButton

  def decrementButton = _decrementButton


  def render(model: CounterModel, actions: Observer[Action]): Unit = {
    counterLabel.setText(model.value.toString)
    incrementButton.setOnAction { () =>
      actions.onNext(Increment(model.id))
    }
    decrementButton.setOnAction { () =>
      actions.onNext(Decrement(model.id))
    }
  }

  def error(err: Throwable): Unit = {
  }

  def complete(): Unit = {
  }

  def dispatch(parentFactory: DispatcherFactory[CounterModel, Action], actions: Observer[Action], changes: Observable[CounterModel]) = {
    val parent = parentFactory({ action => State[CounterModel, Action] { model => Counter.upd(action, model) -> action}})

    val dispatcher = CounterD(parent, actions)

    changes.distinctUntilChanged.subscribe({ model => render(model, actions)}, { err => error(err)}, { () => complete()})
  }

}