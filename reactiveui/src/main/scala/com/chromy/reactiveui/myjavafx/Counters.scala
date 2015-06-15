package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.{FlowPane, HBox}

import com.chromy.reactiveui.Dispatcher
import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.Utils._
import com.chromy.reactiveui.myjavafx.Counters.Add
import monocle.macros.GenLens
import rx.lang.scala.{Observable, Observer, Subscriber}


case class CountersModel(counters: List[CounterModel] = Nil)

case class CountersDispatcher(parent: Dispatcher[CountersModel, Action], actions: Observer[Action], changes: Observable[CountersModel], subscriber: Subscriber[CountersModel]) {
  val counters = GenLens[CountersModel](_.counters)

  changes.distinctUntilChanged.subscribe(subscriber)
}

object Counters {

  case object Add extends Action

  case object Remove extends Action

  def upd(actions: Observer[Action])(model: CountersModel, action: Action): CountersModel = {
    action match {
      case Add => model.copy(counters = CounterModel() :: model.counters)
      case _ => model
    }
  }

  def init() = ???
}

class Counters extends HBox {

  @FXML private var _pAdd: Button = _
  @FXML private var _pRemove: Button = _

  @FXML private var _pCounters: FlowPane = _

  def subscriber(actions: Observer[Action]) = new Subscriber[CountersModel]() {
    override def onNext(model: CountersModel): Unit = {
      _pAdd.setOnAction(() => {
        actions.onNext(Add)
      })

      val l = model.counters.map { in =>
        new Counter()
      }

      _pCounters.getChildren.addAll(l)
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  def dispatch(parentFactory: DispatcherFactory[CountersModel, Action], actions: Observer[Action], changes: Observable[CountersModel]) = {
    //    val parent = parentFactory.subscribe(CounterPair.upd(actions))
    //    val dispatcher = CounterPairDispatcher(parent, actions, changes, Subscriber({ model => render(actions)(model)}, { err => error(err)}, { () => complete()}))

    //    leftCounterController.dispatch(parent.fromLens(dispatcher.leftCounter), actions, changes.map { dispatcher.leftCounter.get })
    //    rightCounterController.dispatch(parent.fromLens(dispatcher.rightCounter), actions, changes.map { dispatcher.rightCounter.get })
  }

}