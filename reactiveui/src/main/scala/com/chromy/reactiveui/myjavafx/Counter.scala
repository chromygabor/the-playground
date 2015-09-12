package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Utils._
import com.chromy.reactiveui.myjavafx.CounterApp.Repository
import rx.lang.scala.{Observer, Subscriber}


case class CounterModel(value: Int = 0, buttonEnabled: Boolean = true, uid: Uid = Uid()) extends Model[Counter]

class Counter(protected val contextMapper: ContextMapper[CounterModel], protected val initialState: CounterModel) extends Component[CounterModel] {

  //val counterStore = Repository.service[CounterStore]
  val counterStore = Repository.ser[CounterStore]

  counterStore.create(initialState.uid)

  override def update(model: ModelType) = Simple {
    case Close(model.uid) =>
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

case class Incremented(id: Uid) extends Action
case class Increment(id: Uid) extends Action

case class Decrement(id: Uid) extends Action

case class Close(id: Uid) extends Action

class CounterController extends GenericJavaFXModule[Counter] {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose
  private[this] var _component: Counter = _

  def subscriber(channel: Observer[Action]): Subscriber[CounterModel] = new Subscriber[CounterModel]() {
    override def onNext(model: CounterModel): Unit = {
      btnIncrement.setDisable(!model.buttonEnabled)
      btnDecrement.setDisable(!model.buttonEnabled)

      lblCounter.setText(s"${model.uid} - ${model.value.toString}")
      btnIncrement.setOnAction { () => channel.onNext(Increment(model.uid)) }
      btnDecrement.setOnAction { () => channel.onNext(Decrement(model.uid)) }
      btnClose.setOnAction { () => channel.onNext(Close(model.uid)) }
    }

    override def onError(error: Throwable): Unit = {
      println("error:")
      error.printStackTrace()
    }

    override def onCompleted(): Unit = super.onCompleted()
  }

  override def dispatch(component: Counter): Counter = {
    _component = component
    _component.subscribe(subscriber(component.channel))
    _component
  }
}
