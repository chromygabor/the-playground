package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Utils._
import monocle.macros.GenLens
import rx.lang.scala.{Observer, Subscriber}

import scala.util.{Failure, Success}


case class CountersModel(counters: List[CounterModel] = List(), uid: Uid = Uid()) extends Model[Counters]

case object Add extends Action


class Counters(protected val routerMapper: RouterMapper[CountersModel], protected val initialState: CountersModel) extends Component[CountersModel] {

  override def upd(model: ModelType): PartialFunction[Action, ModelType] = {
    case Add =>
      model.copy(counters = CounterModel() :: model.counters)
    case Close(uid) =>
      val newCounters = model.counters.filter {
        _.uid != uid
      }
      model.copy(counters = newCounters)
  }

  class ChildrenComponents {
    val counters = ListComponentOf[Counter](router.map(GenLens[CountersModel](_.counters)))((router, state) => new Counter(router.mapper, state))
  }

  val childrenComponents = new ChildrenComponents

}


class CountersController extends GenericJavaFXModule[Counters] {

  @FXML private var _bAdd: Button = _
  lazy val bAdd = _bAdd

  @FXML private var _bRemove: Button = _

  @FXML private var _pCounters: FlowPane = _
  lazy val pCounters = _pCounters

  private var _component: Counters = _

  def subscriber(channel: Observer[Action]): Subscriber[CountersModel] = new Subscriber[CountersModel]() {
    override def onNext(model: CountersModel): Unit = {
      println(s"Counters render: $model on: ${Thread.currentThread.getName}" )
      bAdd.setOnAction { () => channel.onNext(Add) }
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  def listSubscriber = new Subscriber[List[Operation[Counter]]] {
    override def onNext(changes: List[Operation[Counter]]): Unit = {

      val moveItems = changes filter {
        case _: MoveItem[_] => true
        case _ => false
      } map { case in@MoveItem(item, originalIndex, _, _) =>
        (in, pCounters.getChildren.get(originalIndex))
      } reverse

      changes foreach {
        case MoveItem(_, _, computedIndex, _) =>
          pCounters.getChildren.remove(computedIndex)
        case DeleteItem(_, _, computedIndex) =>
          pCounters.getChildren.remove(computedIndex)
        case _ =>
      }

      changes foreach {
        case AddItem(component, index) =>
          JavaFXModule[CounterController](component) match {
            case Success((parent, controller, _)) =>
              pCounters.getChildren.add(index, parent)
            case Failure(e) =>
              e.printStackTrace()
          }
        case _ =>
      }

      moveItems.foreach { case (MoveItem(_, _, _, newIndex), module) =>
        pCounters.getChildren.add(newIndex, module)
      }
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  override def dispatch(component: Counters): Counters = {
    _component = component
    _component.subscribe(subscriber(_component.channel))

    _component.childrenComponents.counters.subscribe(listSubscriber)
    _component
  }
}
