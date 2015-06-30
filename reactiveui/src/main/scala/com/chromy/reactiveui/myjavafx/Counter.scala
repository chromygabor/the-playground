package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.Utils._
import com.chromy.reactiveui.myjavafx.Counter.{ActualState, Close, Decrement, Increment}
import com.chromy.reactiveui.myjavafx.CounterApp.Nop
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Observable, Observer, Subject, Subscriber}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class CounterModel(uid: String = Uid.nextUid().toString, value: Int = 0, buttonEnabled: Boolean = true)

case class CounterDispatcher(parentFactory: DispatcherFactory[CounterModel, Action],
                             private val actionsChannel: Observer[Action], changes: Observable[CounterModel],
                             private val getSubscriber: (Observer[Action]) => Subscriber[CounterModel],
                             initialState: CounterModel = CounterModel()) {
  //  private val parent = parentFactory({ action => State[CounterModel, Action] { model => Counter.upd(action, model) -> action } })

  val actions = Subject[Action]

  val subscriber = getSubscriber(actions)

  val stream = actions.observeOn(ComputationScheduler()).scan((initialState, initialState, Nop.asInstanceOf[Action])) { case ((beforePrevState, prevState, prevAction), action) =>
    Try {
      val newState = Counter.upd(action, prevState, actionsChannel)
      newState
    } match {
      case Success(newState) => (prevState, newState, action)
      case Failure(error) =>
        error.printStackTrace()
        (beforePrevState, prevState, prevAction)
    }
  }

  stream.drop(1).observeOn(JavaFXScheduler()).distinctUntilChanged.subscribe({ input =>
    println(s"input: $input")
    input match {
      case (prevState, actualState, action) =>
        action match {
          case e: LocalAction => subscriber.onNext(actualState)
          case e => actionsChannel.onNext(ActualState(e, actualState))
        }

    }
  })

  private val parent = parentFactory.subscribe { (prevState, action) =>
    println(s"subscriber update: $action")
    action match {
      case ActualState(_, actualState) => actualState
      case e: ActionWrapper => Counter.upd(e.action, prevState, actions)
      case e: Action => Counter.upd(e, prevState, actions)
    }
  }


//  changes.distinctUntilChanged.subscribe({state =>
//    println(s"changes received: $state")
//  })

  changes.distinctUntilChanged.subscribe(subscriber)
}

object Counter extends GenericModule[CounterModel, CounterDispatcher] {
  import scala.concurrent.ExecutionContext.Implicits.global

  case class Increment(id: String) extends LocalAction

  case class Decrement(id: String) extends LocalAction

  case class Close(id: String) extends LocalAction

  case class ActualState(action: Action, state: CounterModel) extends ActionWrapper

  def upd(action: Action, model: CounterModel, actionsChannel: Observer[Action]): CounterModel = action match {
    case Close(model.uid) =>
      model
    case Increment(model.uid) =>
      Future {
        println("Working....")
        Thread.sleep(5000)
        println("Ready....")
        model.copy(value = model.value + 1)
      } map { newState =>
        actionsChannel.onNext(ActualState(Nop, newState))
      }
      model.copy(buttonEnabled = false)
    case Decrement(model.uid) =>
//      Future {
//        Thread.sleep(2000)
//        model.copy(value = model.value - 1)
//      } map { newState =>
//        actionsChannel.onNext(ActualState(Nop, newState))
//      }

      model.copy(buttonEnabled = false)

    case _ => model
  }
}


class Counter extends GenericJavaFXModule[Counter.type] {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose

  def subscriber(changes: Observable[CounterModel]): (Observer[Action]) => Subscriber[CounterModel] = { actions =>
    new Subscriber[CounterModel] {
      override def onNext(model: CounterModel): Unit = {

        println(s"render: $model")

        btnIncrement.setDisable(!model.buttonEnabled)
        btnDecrement.setDisable(!model.buttonEnabled)

        lblCounter.setText(model.value.toString)
        btnIncrement.setOnAction { () => actions.onNext(Increment(model.uid)) }
        btnDecrement.setOnAction { () => actions.onNext(Decrement(model.uid)) }
        btnClose.setOnAction { () => actions.onNext(Close(model.uid)) }
      }

      override def onError(error: Throwable): Unit = super.onError(error)

      override def onCompleted(): Unit = super.onCompleted()
    }
  }

  override def dispatch(parentFactory: DispatcherFactory[CounterModel, Action], actions: Observer[Action], changes: Observable[CounterModel], initialState: CounterModel): CounterDispatcher = {
    CounterDispatcher(parentFactory, actions, changes, subscriber(changes), initialState)
  }
}
