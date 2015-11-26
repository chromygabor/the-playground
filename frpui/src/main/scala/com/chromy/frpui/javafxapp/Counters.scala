package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Utils._
import com.chromy.frpui.fw.javafx.{Controller, JavaFX}
import com.chromy.frpui.fw.util.DiffUtils._
import monocle.macros.GenLens

import scala.util.{Failure, Success}

/**
 * Created by cry on 2015.10.30..
 */
case class Counters(val uid: Uid = Uid(), counters: List[Counter] = Nil) extends Model[Counters] {
  override val children = List(Child(GenLens[Counters](_.counters)))

  override def handle(implicit context: UpdateContext): EventHandler[Counters] = EventHandler {
    case Init => 
      println("Init was called")  //We should fix this
      this
    case CounterAdded(counterUid, value) =>
      copy(counters = ActiveCounter(value = value, uid = counterUid) :: counters)
  }
}

trait MyC[M <: BaseModel] extends Event {
  def isDefinedAt(model: BaseModel): Boolean
  def apply(model: M, context: UpdateContext): Result[M]
}

object Action {
  trait ActionByUid[M <: BaseModel] extends MyC[M] {
    val uid: Uid
    override def isDefinedAt(other: BaseModel): Boolean = other.uid == uid
  }

  trait ActionByType[M <: BaseModel] extends MyC[M] {
    protected val ev: Manifest[M]
    override def isDefinedAt(other: BaseModel): Boolean = ev.runtimeClass.isAssignableFrom(other.getClass)
  }

  def apply[M <: BaseModel : Manifest](f: (M, UpdateContext) => Result[M]): MyC[M] = new MyC[M] {
    private val evidence = manifest[M]
    override def isDefinedAt(other: BaseModel): Boolean = evidence.runtimeClass.isAssignableFrom(other.getClass)
    override def apply(model: M, context: UpdateContext): Result[M] = f(model, context)
  } 
  def apply[M <: BaseModel](uid: Uid)(f: (M, UpdateContext) => Result[M]): MyC[M] = new MyC[M] {
    override def isDefinedAt(other: BaseModel): Boolean = other.uid == uid
    override def apply(model: M, context: UpdateContext): Result[M] = f(model, context)
  }
}


trait Result[M] {
  val model: M
  val event: () => Event
}
object Result {
  def apply[M](iModel: M, iEvent : => Event): Result[M] = new Result[M] {
    override val model: M = iModel
    override val event: () => Event = { () => iEvent }
  }

  def apply[M](iModel: M)(f: (M, UpdateContext) => Event ) : Result[M] = ???
              
//  (iEvent : => Event): Result[M] = new Result[M] {
//    override val model: M = iModel
//    override val event: () => Event = { () => iEvent }
//  }
  
  
  def unapply[M](result: Result[M]): Option[(M, () => Event)] = {
    Some((result.model, result.event))
  }
}

case class AddCounterClicked(uid: Uid) extends Action.ActionByUid[Counters] {
  override def apply(model: Counters, context: UpdateContext) = {
    println(s"AddCounter was called with $model")
    val service = context.getService[CounterService]
    Result(model, service.addCounter2)
  }
}

object Counters extends Behavior[Counters] {
  
  def addCounter2(uid: Uid) = Action[Counters](uid) { (model, context) =>
    Result(model) { (model, context) =>
      val service = context.getService[CounterService]
      service.addCounter()
    }
  }
  
  val addCounter = command { (context, model) =>
    println(s"AddCounter was called with $model")
    val service = context.getService[CounterService]
    service.addCounter()
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
  
  override lazy val renderer = Renderer { (context, model) =>
      SideEffect {
        //bAdd.setOnAction(() => context.call(Counters.addCounter))
        bAdd.setOnAction(() => context.updateContext.onAction(AddCounterClicked(model.uid)))
      }
    } ++ Renderer { (context, prevModel, model) =>
      //println(s"Subscriber get called: $prevModel -> $model")
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
