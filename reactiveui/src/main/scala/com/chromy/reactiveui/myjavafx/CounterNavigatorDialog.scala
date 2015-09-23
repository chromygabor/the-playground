package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField}

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Executable
import com.chromy.reactiveui.core.misc.Utils._
import com.chromy.reactiveui.myjavafx.CounterNavigatorDialog.Increment
import rx.lang.scala.Subscriber

object CounterNavigatorDialog {

  case class Model(uid: Uid = Uid(), value: Integer = 0) extends com.chromy.reactiveui.core.Model[CounterNavigatorDialog]

  def input(in: Option[Uid]): CounterNavigatorDialog.Model = CounterNavigatorDialog.Model()

  case class Increment(uid: Uid) extends Action
}

class CounterNavigatorDialog(protected val contextMapper: ContextMapper[CounterNavigatorDialog.Model]) extends Component[CounterNavigatorDialog.Model]  {
  override protected def update(model: CounterNavigatorDialog.Model): Updater[CounterNavigatorDialog.Model] = Simple {
    case Increment(model.uid) => model.copy(value = model.value + 1)
  }
}

class CounterNavigatorDialogController extends GenericJavaFXController[CounterNavigatorDialog] {
  @FXML private var _bNext: Button = _
  lazy val bNext = _bNext

  @FXML private var _bPrev: Button = _
  lazy val bPrev = _bPrev

  @FXML private var _lValue: TextField = _
  lazy val lValue = _lValue

  private var _component: Component = _

  lazy val subscriber: CounterNavigatorDialog.Model => Executable =  { model => Executable{
      bNext.setText(model.value.toString)
      bNext.setOnAction { () => _component.channel.onNext(Increment(model.uid))}
    }
  }

  override protected def dispatch(component: Component): Component = {
    _component = component

    _component.subscribe(subscriber)
    _component
  }
}

