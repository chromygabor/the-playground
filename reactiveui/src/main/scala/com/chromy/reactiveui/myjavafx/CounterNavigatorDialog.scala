package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.{SideEffect, SideEffect$}
import com.chromy.reactiveui.core.misc.Utils._
import com.chromy.reactiveui.myjavafx.CounterNavigatorDialog.{Up, Down}

object CounterNavigatorDialog {

  case class Model(actual: Option[(Uid, CounterStore.CounterState)] = None, uid: Uid = Uid()) extends com.chromy.reactiveui.core.Model[CounterNavigatorDialog]

  case class Up(uid: Uid) extends Action

  case class Down(uid: Uid) extends Action

}

class CounterNavigatorDialog(protected val contextMapper: ContextMapper[CounterNavigatorDialog.Model]) extends Component[CounterNavigatorDialog.Model]  {
  val counterStore = CounterApp.service[CounterStore]
  counterStore.refresh()

  println("------------------------ "+ this)

  override protected def update(model: ModelType): Updater[ModelType] = Simple {
    case CounterStore.Changed(stateAccessor) =>
      model.actual match {
        case None =>
          println("************ None: " + stateAccessor.firstAvailable(None))
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

  }
}

class CounterNavigatorDialogController extends GenericJavaFXController[CounterNavigatorDialog] {
  @FXML private var _bNext: Button = _
  lazy val bNext = _bNext

  @FXML private var _bPrev: Button = _
  lazy val bPrev = _bPrev

  @FXML private var _lLabel: Label = _
  lazy val lLabel = _lValue

  @FXML private var _lValue: TextField = _
  lazy val lValue = _lValue

  private var _component: Component = _

  lazy val subscriber: CounterNavigatorDialog.Model => SideEffect =  { model =>
    println("++------------------------ "+ _component)
    SideEffect{
      println("*************" + model)

      val (text, textEnabled, hasPrev, hasNext) = model.actual match {
        case None =>
          (" N/A ", false, false, false)
        case Some((uid, counterState)) =>
          (s"$uid: ${counterState.value}", true, counterState.hasPrev, counterState.hasNext)
      }
      lValue.setText(text)

      bNext.setDisable(!hasNext)
      bNext.setOnAction(() => _component.channel.onNext(Up(model.uid)))

      bPrev.setDisable(!hasPrev)
      bPrev.setOnAction(() => _component.channel.onNext(Down(model.uid)))
    }
  }

  override protected def dispatch(component: Component): Component = {
    _component = component
    println("+------------------------ "+ component)

    _component.subscribe(subscriber)
    _component
  }
}

