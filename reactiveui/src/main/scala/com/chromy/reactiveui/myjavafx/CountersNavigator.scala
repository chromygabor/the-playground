package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.myjavafx.CounterApp.Repository
import com.chromy.reactiveui.myjavafx.CounterNavigator.{Open, Down, Up}
import rx.lang.scala.{Observer, Subscriber}
import com.chromy.reactiveui.core.misc.Utils._

/**
 * Created by chrogab on 2015.09.08..
 */
case class CounterNavigatorModel(actual: Option[(Uid, CounterStore.CounterState)] = None, uid: Uid = Uid()) extends Model[CounterNavigator]

object CounterNavigator {

  case class Up(uid: Uid) extends Action

  case class Down(uid: Uid) extends Action

  case class Open(uid: Uid) extends Action

}

class CounterNavigator(val contextMapper: ContextMapper[CounterNavigatorModel], val initialState: CounterNavigatorModel = CounterNavigatorModel()) extends Component[CounterNavigatorModel] {

  //val dialogService = Repository.service[DialogService]

  override protected def update(model: ModelType): Updater[ModelType] = Simple {
    case CounterStore.Changed(stateAccessor) => model.actual match {
      case None => stateAccessor.first match {
        case None => model
        case Some((uid, counterState)) => model.copy(actual = Some((uid, counterState)))
      }
      case Some((uid, counterState)) => stateAccessor(uid) match {
        case None => model
        case state@Some(_) => model.copy(actual = state)
      }
    }
    case Up(model.uid) => (for {
      (uid, counterState) <- model.actual if (counterState.hasNext)
    } yield model.copy(actual = counterState.next)) getOrElse model
    case Down(model.uid) => (for {
      (uid, counterState) <- model.actual if (counterState.hasPrev)
    } yield model.copy(actual = counterState.prev)) getOrElse model

    case Open(model.uid) =>
      model
  }
}

class CounterNavigatorController extends GenericJavaFXModule[CounterNavigator] {
  @FXML private var _bNext: Button = _
  lazy val bNext = _bNext

  @FXML private var _bPrev: Button = _
  lazy val bPrev = _bPrev

  @FXML private var _lValue: Label = _
  lazy val lValue = _lValue

  def subscriber(channel: Observer[Action]): Subscriber[CounterNavigatorModel] = new Subscriber[CounterNavigatorModel]() {
    override def onNext(model: CounterNavigatorModel): Unit = {
      val (text, textEnabled, hasPrev, hasNext) = model.actual match {
        case None =>
          (" N/A ", false, false, false)
        case Some((uid, counterState)) =>
          (s"$uid: ${counterState.value}", true, counterState.hasPrev, counterState.hasNext)
      }
      lValue.setText(text)

      if (textEnabled) {
        lValue.setOnMouseClicked(() => channel.onNext(Open(model.uid)))
      }

      bNext.setDisable(!hasNext)
      bNext.setOnAction(() => channel.onNext(Up(model.uid)))

      bPrev.setDisable(!hasPrev)
      bPrev.setOnAction(() => channel.onNext(Down(model.uid)))
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  private var _component: Component = _

  override def dispatch(component: Component): Component = {
    _component = component
    _component.subscribe(subscriber(_component.channel))
    _component
  }
}
