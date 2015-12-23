package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.{Label, Button}

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.{Renderer, Controller}
import com.chromy.frpui.fw.javafx.Utils._

/**
 * Created by cry on 2015.10.30..
 */

case class CounterNavigatorChanged(newActual: CounterState) extends Event with PersistableEvent

case class CounterNavigator(active: Option[CounterState] = None, uid: Uid = Uid()) extends Model[CounterNavigator]{
  override def handle(implicit context: UpdateContext): EventHandler[CounterNavigator] = EventHandler {
    case Init =>
      CounterNavigator.currentState(this)
    case _: CounterStateChanged =>
      CounterNavigator.currentState(this)
    case CounterNavigatorChanged(active) =>
      copy(active = Some(active))
  }
  
}

object CounterNavigator extends Behavior[CounterNavigator] {

  def currentState(model: CounterNavigator)(implicit context: UpdateContext) ={
    val service = context.getService[CounterService]
    val counterState = model.active match {
      case Some(counter) =>
        service.counter(counter.uid) map { case (uid, counter) => counter }
      case None => 
        context.getService[CounterService].firstAvailable(None).map { case (uid, counter) => counter }
    }
    
    if(model.active != counterState)
      model.copy(active = counterState)
    else
      model
  }

  val next = action { (model, _) =>
    Result(model) { (model, context) =>
      val newActual = context.getService[CounterService].nextAvailable(model.active)
      CounterNavigatorChanged(newActual.map(_._2).get)
    }
  }
  val prev = action { (model, _) =>
    Result(model) { (model, context) =>
      val newActual = context.getService[CounterService].prevAvailable(model.active)
      CounterNavigatorChanged(newActual.map(_._2).get)
    }
  }
}

class CounterNavigatorController extends Controller[CounterNavigator] {
  @FXML var _bNext: Button = _
  lazy val bNext = _bNext
  @FXML var _bPrev: Button = _
  lazy val bPrev = _bPrev
  @FXML var _lValue: Label = _
  lazy val lValue = _lValue
  
  override protected val renderer = Renderer { (context, model) =>
    SideEffect {
      model.active match {
        case None =>
          bPrev.setDisable(true)
          lValue.setText("N/A") 
          bNext.setDisable(true)
        case Some(state) =>
          bPrev.setDisable(!state.hasPrev)
          bPrev.setOnAction { () => context.call(CounterNavigator.prev) }
          lValue.setText(s"${state.uid} - ${state.value.toString}")
          bNext.setDisable(!state.hasNext)
          bNext.setOnAction { () => context.call(CounterNavigator.next)}
      }  
    }
  }
}
