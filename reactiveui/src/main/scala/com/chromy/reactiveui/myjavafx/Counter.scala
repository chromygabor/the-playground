package com.chromy.reactiveui.myjavafx

import java.net.URL
import java.util.{ResourceBundle, UUID}
import javafx.fxml.{FXMLLoader, Initializable, FXML}
import javafx.scene.Parent
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.Dispatcher
import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.Utils._
import com.chromy.reactiveui.myjavafx.Counter.{Close, Decrement, Increment}
import rx.lang.scala.{Subscriber, Observable, Observer}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag

case class CounterModel(uid: String = Uid.nextUid().toString, value: Int = 0)

case class CounterDispatcher(parent: Dispatcher[CounterModel, Action], actions: Observer[Action], changes: Observable[CounterModel], subscriber: Subscriber[CounterModel]) {
  changes.distinctUntilChanged.subscribe(subscriber)
}

object Counter extends GenericModule[CounterModel, CounterDispatcher]{

  case class Increment(id: String) extends LocalAction
  case class Decrement(id: String) extends LocalAction
  case class Close(id: String) extends LocalAction

  def upd(action: Action, model: CounterModel): CounterModel = action match {
    case Close(model.uid) =>
      model
    case Increment(model.uid) =>
      model.copy(value = model.value + 1)
    case Decrement(model.uid) =>
      model.copy(value = model.value - 1)
    case _ => model
  }
}


class Counter extends GenericJavaFXModule[Counter.type] {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose

  var dispatcher: CounterDispatcher = _

  class CounterSubscriber(actions: Observer[Action], changes: Observable[CounterModel]) extends Subscriber[CounterModel] {
    override def onNext(model: CounterModel): Unit = {
      lblCounter.setText(model.value.toString)
      btnIncrement.setOnAction { () => actions.onNext(Increment(model.uid)) }
      btnDecrement.setOnAction { () => actions.onNext(Decrement(model.uid)) }
      btnClose.setOnAction { () => actions.onNext(Close(model.uid))}
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  override def dispatch(parentFactory: DispatcherFactory[CounterModel, Action], actions: Observer[Action], changes: Observable[CounterModel]): CounterDispatcher = {
    val parent = parentFactory({ action => State[CounterModel, Action] { model => Counter.upd(action, model) -> action}})
    dispatcher = CounterDispatcher(parent, actions, changes, new CounterSubscriber(actions, changes))
    dispatcher
  }
}
