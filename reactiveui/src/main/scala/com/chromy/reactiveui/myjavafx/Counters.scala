package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.{FlowPane, HBox}

import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.Utils._
import com.chromy.reactiveui._
import com.chromy.reactiveui.myjavafx.CounterApp.Nop
import com.chromy.reactiveui.myjavafx.Counters.Add
import monocle.Lens
import monocle.macros.GenLens
import rx.lang.scala.{Observable, Observer, Subject, Subscriber}

import scala.collection.immutable.ListMap



case class CountersModel(counters: List[CounterModel] = List())

object Counters extends GenericModule[CountersModel, CountersDispatcher] {

  case object Add extends Action

  def upd(actions: Observer[Action])(model: CountersModel, action: Action): CountersModel = {
    val newModel = action match {
      case Add =>
        model.copy(counters = CounterModel() :: model.counters)
      case Counter.Close(uid) =>
        val splitted = model.counters.splitAt(model.counters.indexWhere(_.uid == uid))
        model.copy(counters = splitted._1 ::: splitted._2.tail)
      case _ => model
    }
    println(s"upd: $model => $newModel")
    newModel
  }
}

case class CountersDispatcher(parent: Dispatcher[CountersModel, Action], actions: Observer[Action], changes: Observable[CountersModel], subscriber: Subscriber[CountersModel]) {
  changes.distinctUntilChanged.subscribe(subscriber)
}


class ListComponent(component: FlowPane, dispatcher: CountersDispatcher, factory: DispatcherFactory[List[CounterModel], Action]) {

  /**
   * Interface
   */
  type UidMap = Map[String, CounterModel]
  def UidMap() = Map[String, CounterModel]()

  def onNext(next: List[CounterModel]): Unit = {
    val map = next.map { in => in.uid -> in}.toMap
    input.onNext(map)
  }

  def initialState: UidMap = UidMap()

  /**
   * Job
   */
  private val input = Subject[UidMap]()

  private val dispatchers = scala.collection.mutable.Map[String, Dispatcher[CounterModel, Action]]()

  factory.subscribe { (newList, action) =>
    newList.map { item =>
      if (dispatchers.get(item.uid).isDefined) {
        dispatchers(item.uid).update(action).run(item)._1
      } else {
        item
      }
    }
  }

  private val myChanges = dispatcher.changes.map {_.counters.map { in => in.uid -> in}.toMap}

  input.scan((initialState, initialState)) { case((beforePrevious, previous), actual) =>
    (previous, actual)
  }.subscribe({in =>
    val (previous, actual) = in

    val diff = ListMatcher.diff(previous.values.toList, actual.values.toList)(_.uid)

    diff.foreach {
      case Removed(index, item: CounterModel) =>
        dispatchers.remove(item.uid)
        component.getChildren.remove(index)
      case Added(index, item: CounterModel) =>
        val disp = Dispatcher[CounterModel, Action]()

        val (nodeToAdd, _, counterDispatcher) = JavaFXFactory[Counter](disp.factory, dispatcher.actions, myChanges.map { change => change(item.uid) })
        dispatchers.update(item.uid, disp)

        component.getChildren.add(index, nodeToAdd)
        //counterDispatcher.subscriber.onNext(item)
    }
  })
}


class Counters extends GenericJavaFXModule[Counters.type] {

  @FXML private var _bAdd: Button = _
  @FXML private var _bRemove: Button = _

  @FXML private var _pCounters: FlowPane = _

  var dispatcher: CountersDispatcher = _

  var myCounters: ListComponent = _

  def bAdd = _bAdd

  def pCounters = _pCounters

  class CountersSubscriber (actions: Observer[Action], changes: Observable[CountersModel]) extends Subscriber[CountersModel]() {
    override def onNext(model: CountersModel): Unit = {
      bAdd.setOnAction(() => actions.onNext(Add))

      myCounters.onNext(model.counters)
    }

    override def onError(error: Throwable): Unit = super.onError(error)

    override def onCompleted(): Unit = super.onCompleted()
  }

  override def dispatch(parentFactory: DispatcherFactory[CountersModel, Action], actions: Observer[Action], changes: Observable[CountersModel]): CountersDispatcher = {
    val parent = parentFactory.subscribe(Counters.upd(actions))
    dispatcher = CountersDispatcher(parent, actions, changes, new CountersSubscriber(actions, changes))

    myCounters = new ListComponent(pCounters, dispatcher, parent.fromLens(GenLens[CountersModel](_.counters)))
    dispatcher
  }
}
