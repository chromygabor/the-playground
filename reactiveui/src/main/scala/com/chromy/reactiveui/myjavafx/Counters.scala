package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.core._
import rx.lang.scala.Observer


case class CountersModel(counters: List[CounterModel] = List(), uid: Uid = Uid()) extends Model[Counters]

class Counters extends BaseComponent[CountersModel] {

  case object Add extends Action

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

  override protected def routerMapper: RouterMapper[CountersModel] = ???

  override def update: (Action, CountersModel, Observer[Action]) => CountersModel = ???

  override protected def initialState: CountersModel = ???
}

//case class CountersDispatcher(parentFactory: DispatcherFactory[CountersModel, Action],
//                              protected val channel: Observer[Action], changes: Observable[CountersModel],
//                              protected val getSubscriber: (Observer[Action]) => Subscriber[CountersModel],
//                              initialState: CountersModel = CountersModel(),
//                              update: (Action, CountersModel, Observer[Action]) => CountersModel = Counters.upd) extends SimpleDispatcher[CountersModel] {

  //  private val dispatchers = scala.collection.mutable.Map[String, Dispatcher[CounterModel, Action]]()
  //
  //  val countersParent = parent.fromLens(GenLens[CountersModel](_.counters))
  //  countersParent.subscribe { (newList, action) =>
  //    newList.map { item =>
  //      if (dispatchers.get(item.uid).isDefined) {
  //        dispatchers(item.uid).update(action).run(item)._1
  //      } else {
  //        item
  //      }
  //    }
  //    newList
  //  }
//}


class CountersController extends GenericJavaFXModule[Counters] {

  @FXML private var _bAdd: Button = _
  @FXML private var _bRemove: Button = _

  @FXML private var _pCounters: FlowPane = _

//  var dispatcher: CountersDispatcher = _
//
//  lazy val bAdd = _bAdd
//
//  lazy val pCounters = new ListComponent(_pCounters, dispatcher, dispatcher.parent.fromLens(GenLens[CountersModel](_.counters)))
//
//  def subscriber(changes: Observable[CountersModel]): (Observer[Action]) => Subscriber[CountersModel] = { actions =>
//    new Subscriber[CountersModel] {
//      override def onNext(model: CountersModel): Unit = {
//        bAdd.setOnAction(() => actions.onNext(Add))
//
//        pCounters.onNext(model.counters)
//      }
//
//      override def onError(error: Throwable): Unit = super.onError(error)
//
//      override def onCompleted(): Unit = super.onCompleted()
//    }
//  }
//
//
//  override def dispatch(parentFactory: DispatcherFactory[CountersModel, Action], actions: Observer[Action], changes: Observable[CountersModel], initialState: CountersModel): CountersDispatcher = {
//    dispatcher = CountersDispatcher(parentFactory, actions, changes, subscriber(changes))
//
//    dispatcher.init()
//    dispatcher
//  }
}
