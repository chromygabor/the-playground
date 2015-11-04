package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.frpui.Renderer
import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Controller
import com.chromy.frpui.fw.javafx.Utils._

/**
 * Created by cry on 2015.10.30..
 */

trait Counter extends Model[Counter] {
  val value: Int
}

case class ExistingCounter(value: Int, uid: Uid = Uid()) extends Counter

case class NotExistingCounter(value: Int, uid: Uid = Uid()) extends Counter

object Counter extends Behavior[Counter] {
  def increment = Action { (_, model) =>
    model match {
      case model: ExistingCounter => model.copy(model.value + 1)
      case _ => model
    }
  }

  def decrement = Action { (_, model) =>
    model match {
      case model: ExistingCounter => model.copy(model.value - 1)
      case _ => model
    }
  }

  def close = Action { (_, model) =>
    NotExistingCounter(0, model.uid)
  }

  def apply() = ExistingCounter(0)
}

class CounterController extends Controller[Counter] {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose

  override lazy val renderer: Renderer[Counter] = Renderer()
    Renderer { implicit model =>
      SideEffect {
        lblCounter.setText(model.value.toString)

        btnIncrement.setOnAction(() => fire(Counter.increment))

        btnDecrement.setOnAction(() => fire(Counter.decrement))

        btnClose.setOnAction(() => fire(Counter.close))
      }
    }
  
}
