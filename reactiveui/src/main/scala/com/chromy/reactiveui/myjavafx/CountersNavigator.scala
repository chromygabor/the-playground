package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Executable
import com.chromy.reactiveui.core.misc.Utils._
import com.chromy.reactiveui.myjavafx.CounterNavigator.{Down, Open, Up}
import rx.lang.scala.{Observer, Subscriber}

/**
 * Created by chrogab on 2015.09.08..
 */
case class CounterNavigatorModel(actual: Option[(Uid, CounterStore.CounterState)] = None, uid: Uid = Uid()) extends Model[CounterNavigator]

object CounterNavigator {

  case class Up(uid: Uid) extends Action

  case class Down(uid: Uid) extends Action

  case class Open(uid: Uid) extends Action

}

class CounterNavigator(val contextMapper: ContextMapper[CounterNavigatorModel]) extends Component[CounterNavigatorModel] {

  val dialogService = CounterApp.service[DialogService]

  def firstAvailable(actual: Option[(Uid, CounterStore.CounterState)]): Option[(Uid, CounterStore.CounterState)] = ???

  override protected def update(model: ModelType): Updater[ModelType] = Simple {
    case CounterStore.Changed(stateAccessor) =>
      model.actual match {
        case None =>
          model.copy(actual = stateAccessor.firstAvailable(None))
        case Some((uid, counterState)) => stateAccessor(uid) match {
          case None =>
            model.copy(actual = stateAccessor.firstAvailable(model.actual))
          case state@Some(_) =>
            model.copy(actual = state)
        }
      }
    case Up(model.uid) =>
      (for {
        (uid, counterState) <- model.actual if (counterState.hasNext)
      } yield model.copy(actual = counterState.next)) getOrElse model
    case Down(model.uid) =>
      (for {
        (uid, counterState) <- model.actual if (counterState.hasPrev)
      } yield model.copy(actual = counterState.prev)) getOrElse model

    case Open(model.uid) =>
      dialogService.open[CounterNavigatorDialog](model.uid, CounterNavigatorDialog.Model())
      model
  }
}

class CounterNavigatorController extends GenericJavaFXController[CounterNavigator] with DialogServiceController {
  @FXML private var _bNext: Button = _
  lazy val bNext = _bNext

  @FXML private var _bPrev: Button = _
  lazy val bPrev = _bPrev

  @FXML private var _lValue: Label = _
  lazy val lValue = _lValue

  private var _component: Component = _

  lazy val subscriber: CounterNavigatorModel =>  Executable =  { model => Executable {
      val (text, textEnabled, hasPrev, hasNext) = model.actual match {
        case None =>
          (" N/A ", false, false, false)
        case Some((uid, counterState)) =>
          (s"$uid: ${counterState.value}", true, counterState.hasPrev, counterState.hasNext)
      }
      lValue.setText(text)

      if (textEnabled) {
        lValue.setOnMouseClicked(() => _component.channel.onNext(Open(model.uid)))
      }

      bNext.setDisable(!hasNext)
      bNext.setOnAction(() => _component.channel.onNext(Up(model.uid)))

      bPrev.setDisable(!hasPrev)
      bPrev.setOnAction(() => _component.channel.onNext(Down(model.uid)))
    }
  }

  override protected def dispatch(component: Component): Component = {
    _component = component
    _component.subscribe(subscriber)
    _component
  }
}
