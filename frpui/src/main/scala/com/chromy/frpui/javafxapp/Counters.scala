package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Utils._
import com.chromy.frpui.fw.javafx.{Controller, JavaFX, Utils}
import com.chromy.frpui.fw.util.DiffUtils._
import monocle.macros.GenLens

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by cry on 2015.10.30..
 */
case class Counters(val uid: Uid = Uid(), counters: List[Counter] = Nil) extends Model[Counters] {
  override val children = List(Child(GenLens[Counters](_.counters)))

  override protected def handle(implicit context: Context): EventHandler[Counters] = EventHandler{
    case Init => 
      val service = context.getService[CounterService]
      
      this
    case Close(uid) => copy(counters = counters.filter( _.uid != uid ))
  }
}

object Counters extends Behavior[Counters] {
  private def resultReceived = Action { (contex, model) =>
    println(s"ResultReceived: $model")
    model.copy(counters = Counter() :: model.counters)
  }

  def addClicked = Action { (context, model) =>
    import scala.concurrent.ExecutionContext.Implicits.global

    Future {
      println("Sleeping for 1 sec")
      Thread.sleep(100)
      println("Sleeping is over")
      context.onAction(resultReceived)
    }
    model
  }
}


class CountersController extends Controller[Counters] {
  @FXML private var _bAdd: Button = _
  lazy val bAdd = _bAdd

  @FXML private var _pCounters: FlowPane = _
  lazy val pCounters = _pCounters

  //  @FXML private var _counterNavigatorController: CounterNavigatorController = _
  //  lazy val counterNavigator = _counterNavigatorController

  private lazy val subrenderer = render.map { model =>
    model.counters.map { item =>
      item.uid -> item
    }.toMap
  }
  
  override lazy val renderer =
    Renderer { implicit model =>
      SideEffect {
        //println(s"SideEffect (C) get run")
        bAdd.setOnAction(() => onAction(Counters.addClicked))
      }
    } ++ Renderer { (prevModel, model) =>
      //println(s"Subscriber get called: $prevModel -> $model")
      val lcounters = prevModel.counters.diffOps(model.counters)(_.uid)
      val counters = lcounters.map {
        case Insert(pos, item) =>
          val component = JavaFX[CounterController](channel, subrenderer.keyOption(item.uid), item)
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
                  println("Couldn't add counter")
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
