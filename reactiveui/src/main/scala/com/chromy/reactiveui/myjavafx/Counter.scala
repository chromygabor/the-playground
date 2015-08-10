package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Utils._
import rx.lang.scala.{Observer, Subscriber}


case class CounterModel(value: Int = 0, buttonEnabled: Boolean = true, uid: Uid = Uid()) extends Model[Counter]

class Counter(protected val routerMapper: RouterMapper[CounterModel], protected val initialState: CounterModel) extends Component[CounterModel] {
  override def update: (Action, ModelType, Observer[Action]) => ModelType = { (action, model, _) => model }

  override def toString = s"Counter(${initialState.uid})"
}

case class Increment(id: Uid) extends LocalAction

case class Decrement(id: Uid) extends Action

case class Close(id: Uid) extends Action

//
//case class StateChange[T](action: Action, state: T) extends ActionWrapper
//
//object Counter extends GenericModule[CounterModel, CounterDispatcher] {
//  import scala.concurrent.ExecutionContext.Implicits.global
//
//  case class Increment(id: String) extends LocalAction
//
//  case class Decrement(id: String) extends Action
//
//  case class Close(id: String) extends Action
//
//  def upd(action: Action, model: CounterModel, actionsChannel: Observer[Action]): CounterModel = action match {
//    case Close(model.uid) =>
//      model
//    case Increment(model.uid) =>
//      Future {
//        println("Working....")
//        Thread.sleep(3000)
//        println("Ready....")
//        model.copy(value = model.value + 1)
//      } map { newState =>
//        actionsChannel.onNext(StateChange(action, newState))
//      }
//      model.copy(buttonEnabled = false)
//    case Decrement(model.uid) =>
////      Future {
////        Thread.sleep(2000)
////        model.copy(value = model.value - 1)
////      } map { newState =>
////        actionsChannel.onNext(ActualState(Nop, newState))
////      }
//
//      model.copy(value = model.value - 1)
//
//    case _ => model
//  }
//}
//
//
class CounterController extends GenericJavaFXModule[Counter] {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose

  def subscriber(channel: Observer[Action]): Subscriber[CounterModel] = new Subscriber[CounterModel]() {
    override def onNext(model: CounterModel): Unit = {

      btnIncrement.setDisable(!model.buttonEnabled)
      btnDecrement.setDisable(!model.buttonEnabled)

      lblCounter.setText(s"${model.uid} - ${model.value.toString}")
      btnIncrement.setOnAction { () => channel.onNext(Increment(model.uid)) }
      btnDecrement.setOnAction { () => channel.onNext(Decrement(model.uid)) }
      btnClose.setOnAction { () => channel.onNext(Close(model.uid)) }
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  override def dispatch(component: Counter): Counter = {
    component.subscribe(subscriber(component.channel))
    component
  }
}
