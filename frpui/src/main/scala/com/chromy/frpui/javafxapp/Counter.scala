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

case class ActiveCounter(value: Int, uid: Uid = Uid()) extends Counter {
  override def handle(implicit context: Context): EventHandler[Counter] = Counter.eventHandler(this)
}

case class DeletedCounter(value: Int, uid: Uid = Uid()) extends Counter

object Counter extends Behavior[Counter] {

  override def handler(model: Counter) = Handler {
    case Init => init
    case CountersChanged(stateAccessor) if stateAccessor(model.uid).isDefined  => countersChanged(stateAccessor(model.uid).get._2)
  }

  def countersChanged(state: CounterState) = Action { (context: Context, model) =>
      model match {
        case m: ActiveCounter if m.value != state.value =>
          println(s"Counter changed: ${model.uid}, ${state.value}")
          m.copy(value = state.value)
        case m => m
      }
  }

  val init = Action { (context: Context, model) =>
    val service = context.getService[CounterService]
    service.subscribe(model.uid)
    println("Init")
    model
  }

  val increment = Action { (context, model) =>
    println(s"Increment: $model")
    val service = context.getService[CounterService]
    model match {
      case model: ActiveCounter =>
        service.increment(model.uid)
        model
      case _ => model
    }
  }

  val decrement = Action { (context, model) =>
    println(s"Decrement: $model")
    val service = context.getService[CounterService]
    model match {
      case model: ActiveCounter =>
        service.increment(model.uid)
        model
      case _ => model
    }
  }

  val close = PostOrderAction { (_, model) =>
    println("Closing....")
    DeletedCounter(0, model.uid)
  }

  def apply() = ActiveCounter(-1)
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

  override lazy val renderer: Renderer[Counter] = Renderer { implicit model =>
      SideEffect {
        lblCounter.setText(s"${model.uid} ${model.value.toString}")

        btnIncrement.setOnAction(() => onAction(Counter.increment))

        btnDecrement.setOnAction(() => onAction(Counter.decrement))

        btnClose.setOnAction(() => onAction(Counter.close))
      }
    }
  
}
