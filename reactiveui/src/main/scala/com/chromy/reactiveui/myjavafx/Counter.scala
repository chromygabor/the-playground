package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Executable
import com.chromy.reactiveui.core.misc.Utils._
import com.chromy.reactiveui.myjavafx.Counter._
import rx.lang.scala.{Observer, Subscriber}


case class CounterModel(value: Int = 0, buttonEnabled: Boolean = true, uid: Uid = Uid()) extends Model[Counter]

object Counter {
  case class Increment(id: Uid) extends Action
  case class Decrement(id: Uid) extends Action
  case class Close(id: Uid) extends Action
  case class Removed(id: Uid) extends Action
}

class Counter(protected val contextMapper: ContextMapper[CounterModel]) extends Component[CounterModel] {

  val counterStore = CounterApp.service[CounterStore]

  counterStore.create(initialState.uid)

  override def update(model: ModelType) = Simple {
    case Close(model.uid) =>
      counterStore.remove(model.uid)
      channel.onNext(Removed(uid))
      model
    case Increment(model.uid) =>
      counterStore.increment(model.uid)
      model
    case Decrement(model.uid) =>
      counterStore.decrement(model.uid)
      model
    case CounterStore.Changed(stateAccessor) =>
      val (_, counterState) = stateAccessor(model.uid).get

      model.copy(value = counterState.value)
  }
  override def toString = s"Counter(${initialState.uid})"

}

class CounterController extends GenericJavaFXController[Counter] {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose
  private[this] var _component: Counter = _

  lazy val subscriber: CounterModel => Executable =  { model => Executable {
    btnIncrement.setDisable(!model.buttonEnabled)
    btnDecrement.setDisable(!model.buttonEnabled)

    lblCounter.setText(s"${model.uid} - ${model.value.toString}")
    btnIncrement.setOnAction { () => _component.channel.onNext(Increment(model.uid)) }
    btnDecrement.setOnAction { () => _component.channel.onNext(Decrement(model.uid)) }
    btnClose.setOnAction { () => _component.channel.onNext(Close(model.uid)) }
  }}

  override def dispatch(component: Counter): Counter = {
    _component = component
    _component.subscribe(subscriber)
    _component
  }
}
