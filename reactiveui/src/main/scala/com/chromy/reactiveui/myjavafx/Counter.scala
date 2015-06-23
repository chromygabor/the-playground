package com.chromy.reactiveui.myjavafx

import java.net.URL
import java.util.{ResourceBundle, UUID}
import javafx.fxml.{Initializable, FXML}
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.Dispatcher
import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.Utils._
import com.chromy.reactiveui.myjavafx.Counter.{Close, Decrement, Increment}
import rx.lang.scala.{Subscriber, Observable, Observer}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Uid {
  private var _id: Int = 0

  def nextUid() = {
    val res = _id
    _id = _id + 1
    res
  }
}

case class CounterModel(uid: String = Uid.nextUid().toString, value: Int = 0){
  val id: String = UUID.randomUUID().toString
}

case class CounterD(parent: Dispatcher[CounterModel, Action], actions: Observer[Action], changes: Observable[CounterModel], subscriber: Subscriber[CounterModel]) {
  changes.distinctUntilChanged.subscribe(subscriber)
}

object Counter extends Module[CounterModel, Counter]{

  case class Increment(id: String) extends LocalAction

  case class Decrement(id: String) extends LocalAction
  case class Close(id: String) extends LocalAction

  def upd(action: Action, model: CounterModel): CounterModel = action match {
    case Increment(model.id) =>
      println(s"update: $model.uid")
      model.copy(value = model.value + 1)
    case Decrement(model.id) =>
      println(s"update: $model.uid")
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


class Counter extends Initializable {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose

  var dispatcher: CounterD = _

  def subscriber(actions: Observer[Action], changes: Observable[CounterModel]) = new Subscriber[CounterModel]() {
    override def onNext(model: CounterModel): Unit = {
      lblCounter.setText(model.value.toString)
      btnIncrement.setOnAction { () => actions.onNext(Increment(model.id)) }
      btnDecrement.setOnAction { () => actions.onNext(Decrement(model.id)) }
      btnClose.setOnAction { () => actions.onNext(Close(model.uid))}
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }


  def dispatch(parentFactory: DispatcherFactory[CounterModel, Action], actions: Observer[Action], changes: Observable[CounterModel]): CounterD = {
    val parent = parentFactory({ action => State[CounterModel, Action] { model => Counter.upd(action, model) -> action}})
    dispatcher = CounterD(parent, actions, changes, subscriber(actions, changes))
    dispatcher
  }

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    
  }
}
