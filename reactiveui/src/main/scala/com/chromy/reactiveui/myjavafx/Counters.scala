package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Utils._
import monocle.macros.GenLens
import rx.lang.scala.{Subscriber, Observer}


case class CountersModel(counters: List[CounterModel] = List(), uid: Uid = Uid()) extends Model[Counters]

case object Add extends Action


class Counters(protected val routerMapper: RouterMapper[CountersModel], protected val initialState: CountersModel) extends Component[CountersModel] {

  //  def upd(action: Action, model: CountersModel, actionsChannel: Observer[Action]): CountersModel = {
  //    val newModel = action match {
  //      case Add =>
  //        model.copy(counters = CounterModel() :: model.counters)
  //      case Counter.Close(uid) =>
  //        val splitted = model.counters.splitAt(model.counters.indexWhere(_.uid == uid))
  //        model.copy(counters = splitted._1 ::: splitted._2.tail)
  //      case _ => model
  //    }
  //    newModel
  //  }

  override def update: (Action, CountersModel, Observer[Action]) => CountersModel = { (action, model, channel) =>
    action match {
      case Add => model.copy(counters = CounterModel() :: model.counters)
      case _ => model
    }
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
      bAdd.setOnAction{() => channel.onNext(Add)}
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  def listSubscriber = new Subscriber[Operation[Counter]] {
    override def onNext(change: Operation[Counter]): Unit = {
      println(s"list change: $change")

      change match {
        case AddItem(component, index) =>
          val (parent, controller, _) = JavaFXModule[CounterController](component)
          println(s"adding $component")
//          pCounters.getChildren.add(parent)
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
