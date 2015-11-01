package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Utils._
import com.chromy.frpui.fw.javafx.{Controller, JavaFX, Utils}
import com.chromy.frpui.fw.util.DiffUtils._

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by cry on 2015.10.30..
 */
case class Counters(val uid: Uid = Uid(), counters: List[Counter] = Nil) extends Model[Counters] 

object Counters extends Behavior[Counters] {
  private def resultReceived = Action { (contex, model) =>
    println(s"ResultReceived: $model")
    model.copy(counters = Counter() :: model.counters)
  }

  def addClicked = Action { (context, model) =>
    import scala.concurrent.ExecutionContext.Implicits.global

    Future {
      println("Sleeping for 1 sec")
      Thread.sleep(2000)
      println("Sleeping is over")
      context.fire(resultReceived)
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

  override lazy val renderer = Renderer { implicit model =>
    SideEffect {
      println(s"SideEffect (C) get run")
      bAdd.setOnAction(() => fire(Counters.addClicked))
    }
  } ++
    Renderer { (prevModel, model) =>
      println(s"Subscriber get called: $prevModel -> $model")
      val counters = prevModel.counters.diffOps(model.counters)(_.uid).map {
        case Insert(pos, item) =>
          val component = JavaFX[CounterController](channel, render.map(_.counters).indexOption(pos), item)
          Insert(pos, component)
        case Remove(pos) => Remove(pos)
        case Move(from, to) => Move(from, to)
      }

      SideEffect {
        println(s"SideEffect (C,C) get run")

        counters.foreach {
          case Insert(pos, component) => component match {
            case Success((parent, _)) =>
              pCounters.getChildren.add(parent)
            case Failure(error) =>
              println("Couldn't add counter")
              error.printStackTrace()
          }

          case Remove(pos) =>
            pCounters.getChildren.remove(pos)
          case Move(from, to) =>
            val component = pCounters.getChildren.get(from)
            pCounters.getChildren.remove(from)
            pCounters.getChildren.add(to, component)
        }
      }
    }

}
