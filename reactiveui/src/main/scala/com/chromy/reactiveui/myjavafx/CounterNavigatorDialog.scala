package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{TextField, Label, Button}

import com.chromy.reactiveui.core.{UiModel, ContextMapper, Uid, Component}
import com.chromy.reactiveui.myjavafx.CounterNavigatorDialog.Model

/**
 * Created by chrogab on 2015.09.17..
 */

trait DialogComponent[A <: UiModel] {
  def stateFromInput[B](in: B): A
}

object CounterNavigatorDialog {

  case class Input(text: String)

  case class Output(text: String)

  case class Model(uid: Uid = Uid()) extends com.chromy.reactiveui.core.Model[CounterNavigatorDialog]

}

class CounterNavigatorDialog(protected val contextMapper: ContextMapper[CounterNavigatorDialog.Model], protected val initialState: CounterNavigatorDialog.Model) extends Component[CounterNavigatorDialog.Model]
with DialogComponent[CounterNavigatorDialog.Model] {
  override def stateFromInput[B](in: B): Model = ???

}

class CounterNavigatorDialogController extends GenericJavaFXModule[CounterNavigatorDialog] {
  @FXML private var _bNext: Button = _
  lazy val bNext = _bNext

  @FXML private var _bPrev: Button = _
  lazy val bPrev = _bPrev

  @FXML private var _lValue: TextField = _
  lazy val lValue = _lValue

  private var _component: Component = _

  override protected def dispatch(component: Component): Component = {
    _component = component
    _component
  }
}

