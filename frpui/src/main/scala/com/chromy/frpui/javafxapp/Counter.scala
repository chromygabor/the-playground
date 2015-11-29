package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Controller
import com.chromy.frpui.fw.javafx.Utils._


/**
 * Created by cry on 2015.10.30..
 */

trait Counter extends Model[Counter] {
  val value: Int
}

case class ActiveCounter(value: Int, uid: Uid = Uid()) extends Counter {
  override def handle(implicit context: UpdateContext): EventHandler[Counter] = EventHandler {
    case CounterChanged(`uid`, value) =>
      copy(value = value)
  }
}

case class DeletedCounter(value: Int, uid: Uid = Uid()) extends Counter

object Counter extends Behavior[Counter] {
  val increment = action { (model, context) =>
    Result(model) { (_, _) =>
      val service = context.getService[CounterService]
      service.increment(model.uid)
    }
  }

  val decrement = action { (model, context) =>
    Result(model) { (_, _) =>
      val service = context.getService[CounterService]
      service.decrement(model.uid)
    }
  }
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

  override lazy val renderer = Renderer { (context, model) =>
    SideEffect {
      lblCounter.setText(s"${model.uid} ${model.value.toString}")

      btnIncrement.setOnAction(() => context.call(Counter.increment))

      btnDecrement.setOnAction(() => context.call(Counter.decrement))

      //btnClose.setOnAction(() => onAction(Counter.close))
    }
  }

}
