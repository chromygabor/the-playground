package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.core._
import com.msci.playground.StateMonadPlayground.Add
import monocle.macros.GenLens
import rx.lang.scala.{Subscriber, Observer}
import com.chromy.reactiveui.core.misc.Utils._


case class CountersModel(counters: List[CounterModel] = List(), uid: Uid = Uid()) extends Model[Counters]

case object Add extends Action


class Counters(protected val routerMapper: RouterMapper[CountersModel], protected val initialState: CountersModel) extends BaseComponent[CountersModel] {

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
    val counters = ListComponentOf[CounterModel](router.map(GenLens[CountersModel](_.counters)))(router => new Counter(router.mapper, CounterModel()))
  }

  val childrenComponents = new ChildrenComponents

}


class CountersController extends GenericJavaFXModule[Counters] {

  @FXML private var _bAdd: Button = _
  @FXML private var _bRemove: Button = _

  @FXML private var _pCounters: FlowPane = _

  private var _component: Counters = _

  def subscriber(channel: Observer[Action]): Subscriber[CountersModel] = new Subscriber[CountersModel]() {
    override def onNext(model: CountersModel): Unit = {
      _bAdd.setOnAction(() => channel.onNext(Add))
      // pCounters.onNext(model.counters)
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  def listSubscriber(channel: Observer[Action]): Subscriber[Operation[CounterModel]] = {
    ???
  }

  override def dispatch(routerMapper: RouterMapper[CountersModel], initialState: CountersModel): Unit = {
    _component = new Counters(routerMapper, initialState)
    _component.router.changes.subscribe(subscriber(_component.router.channel))

    _component.childrenComponents.counters.router.changes.subscribe({ item =>
      println("changed: item")
    })
  }
}
