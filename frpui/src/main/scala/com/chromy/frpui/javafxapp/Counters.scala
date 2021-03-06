package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Utils._
import com.chromy.frpui.fw.javafx.{ChildController, BaseController, Controller, JavaFX}
import com.chromy.frpui.fw.util.DiffUtils._
import monocle._
import monocle.macros.GenLens

import scala.util.{Failure, Success}

/**
 * Created by cry on 2015.10.30..
 */
case class Counters(val uid: Uid = Uid(), counters: List[Counter] = Nil, counterNavigator: CounterNavigator = CounterNavigator()) extends Model[Counters] {
  override val children = List(
    Child(GenLens[Counters](_.counters)),
    Child(GenLens[Counters](_.counterNavigator))
  )

  override def handle(implicit context: UpdateContext): EventHandler[Counters] = EventHandler {
    case Init => 
      this
    case CounterAdded(counterUid, value) =>
      copy(counters = Counter(value = value, uid = counterUid) :: counters)
    case CounterRemoved(counterUid) =>
      copy(counters = counters.filterNot(_.uid == counterUid))
  }
}


object Counters extends Behavior[Counters] {
  
  val addCounterClicked = action { (model, context) =>
    Result(model) { (_, _) =>
      val service = context.getService[CounterService]
      service.addCounter
    }
  }
}


class CountersController extends Controller[Counters] {
  @FXML private var _bAdd: Button = _
  lazy val bAdd = _bAdd

  @FXML private var _pCounters: FlowPane = _
  lazy val pCounters = _pCounters

  @FXML private var _counterNavigatorController: CounterNavigatorController = _
  lazy val counterNavigator = _counterNavigatorController

  override lazy val children = List(
    child(counterNavigator){_.counterNavigator}
  )
  
  private lazy val subrenderer = render.map { model =>
    model.counters.map { item =>
      item.uid -> item
    }.toMap
  }
  
  override lazy val renderer = Renderer { (context, model) =>
      SideEffect {
        bAdd.setOnAction(() => context.call(Counters.addCounterClicked))
      }
    } ++ Renderer { (context, prevModel, model) =>
      val lcounters = prevModel.counters.diffOps(model.counters)(_.uid)
      val counters = lcounters.map {
        case Insert(pos, item) =>
          val component = JavaFX[CounterController](context, subrenderer.keyOption(item.uid), item)
          Insert(pos, component)
        case Remove(pos) => Remove(pos)
        case Move(from, to) => Move(from, to)
      }

      counters.foldLeft(SideEffect()) { (accu, counter) =>
        counter match {
          case Insert(pos, component) => component match {
            case Success((parent, _, effect)) =>
              accu ++ effect + {  
                pCounters.getChildren.add(pos,parent)
              }
            case Failure(error) =>
              accu + { 
                  error.printStackTrace()
              }
          }

          case Remove(pos) =>
            accu + { 
              pCounters.getChildren.remove(pos)
            }
          case Move(from, to) =>
            accu + {
                val component = pCounters.getChildren.get(from)
                pCounters.getChildren.remove(from)
                pCounters.getChildren.add(to, component)
            }
        }
      }
    }
}
