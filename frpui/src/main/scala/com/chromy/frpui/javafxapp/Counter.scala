package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Controller
import com.chromy.frpui.fw.javafx.Utils._


/**
 * Created by cry on 2015.10.30..
 */


case class Counter(value: Int, uid: Uid = Uid(), enabled: Boolean = true) extends Model[Counter] {
  override def handle(implicit context: UpdateContext): EventHandler[Counter] = EventHandler {
    case CounterChanged(`uid`, value) =>
      copy(value = value)
  }
}

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
  
  val close = action { (model, context) =>
    Result(model) { (_, _) =>
      val service = context.getService[CounterService]
      service.close(model.uid)
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

      btnClose.setOnAction(() => context.call(Counter.close))
    }
  }

}
