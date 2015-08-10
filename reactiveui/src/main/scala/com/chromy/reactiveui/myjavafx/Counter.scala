package com.chromy.reactiveui.myjavafx
import com.chromy.reactiveui.core._
import rx.lang.scala.Observer

//
//import javafx.fxml.FXML
//import javafx.scene.control.{Button, Label}
//
//import com.chromy.reactiveui.core.Dispatcher
//import Dispatcher.DispatcherFactory
//import com.chromy.reactiveui.core.misc.Utils
//import Utils._
//import com.chromy.reactiveui.myjavafx.Action
//import com.chromy.reactiveui.myjavafx.Counter.{Close, Decrement, Increment}
//import com.chromy.reactiveui.myjavafx.CounterApp.Nop
//import rx.lang.scala.schedulers.ComputationScheduler
//import rx.lang.scala.{Observable, Observer, Subject, Subscriber}
//
//import scala.concurrent.Future
//import scala.util.{Failure, Success, Try}
//
case class CounterModel(value: Int = 0, buttonEnabled: Boolean = true, uid: Uid = Uid()) extends Model[Counter]

class Counter(protected val routerMapper: RouterMapper[CounterModel], protected val initialState: CounterModel)  extends BaseComponent[CounterModel] {
  override def update: (Action, ModelType, Observer[Action]) => ModelType = { (action, model, _) => model}

  override def toString = s"Counter(${initialState.uid})"
}
//
//case class CounterDispatcher(parentFactory: DispatcherFactory[CounterModel, Action],
//                             protected val channel: Observer[Action], changes: Observable[CounterModel],
//                             protected val getSubscriber: (Observer[Action]) => Subscriber[CounterModel],
//                             initialState: CounterModel = CounterModel(),
//                             update: (Action, CounterModel, Observer[Action]) => CounterModel = Counter.upd ) extends SimpleDispatcher[CounterModel]
//
//object SimpleDispatcher {
//  type State = { def uid: String}
//
//}
//
//trait SimpleDispatcher[T <: SimpleDispatcher.State] {
//  val parentFactory: DispatcherFactory[T, Action]
//  protected val channel: Observer[Action]
//  val changes: Observable[T]
//  protected val getSubscriber: (Observer[Action]) => Subscriber[T]
//  val initialState: T
//  def update: (Action, T, Observer[Action]) => T
//
//  private lazy val name =  s"DSP-${this.getClass.getSimpleName}(${initialState.uid})"
//  println(s"[$name] created ")
//
//  case class ActualState(action: Action, state: T) extends ActionWrapper
//  case class Step(action:Action, state: T) extends Action
//
//  val actions = Subject[Action]
//
//  val render = Subject[T]
//
//  lazy val subscriber = new Subscriber[T]() {
//    val dest = getSubscriber(actions)
//    override def onNext(value: T): Unit = {
//      println(s"[$name] rendering: $value ")
//      dest.onNext(value)
//    }
//
//    override def onError(error: Throwable): Unit = {
//      dest.onError(error)
//    }
//
//    override def onCompleted(): Unit = {
//      dest.onCompleted()
//    }
//  }
//
//
//  val stream = actions.observeOn(ComputationScheduler()).scan((initialState, initialState, Nop.asInstanceOf[Action])) { case ((beforePrevState, prevState, prevAction), action) =>
//    Try[T] {
//      action match {
//        case Step(action, newState) =>
//          newState
//        case StateChange(actionToWrap, actualState: T) =>
//          println(s"=================== [$name] action received  $action =================")
//          println(s"[$name] action is ActualState $actualState")
//          actualState
//        case action: ActionWrapper =>
//          println(s"[$name] action is already a WrapperAction so return the previous state")
//          prevState
//        case action =>
//          println(s"=================== [$name] action received  $action =================")
//          val newState = update(action, prevState, actions)
//          println(s"[$name] action triggered a new state: $prevState => $newState")
//          newState
//      }
//    } match {
//      case Success(newState) => (prevState, newState, action)
//      case Failure(error) =>
//        error.printStackTrace()
//        (beforePrevState, prevState, prevAction)
//    }
//  }
//
//  stream.drop(1).observeOn(JavaFXScheduler()).distinctUntilChanged.subscribe({ input =>
//    input match {
//      case (prevState, actualState, action) =>
//        action match {
//          case Step(_, _) =>
//            println(s"[$name] action is a step so nothing to do: $action")
//          case e: LocalAction =>
//            println(s"[$name] action is LocalAction triggering render: $actualState")
//            subscriber.onNext(actualState)
//          case action@StateChange(a, s: T) =>
//            val wrap = ActualState(a,s)
//            println(s"[$name] action is StateChange sending to channel as a $wrap")
//            channel.onNext(wrap)
//          case action: ActionWrapper =>
//            println(s"[$name] action is already a WrapperAction so just send up to channel: ${action}")
//            channel.onNext(action)
//          case action =>
//            println(s"[$name] action is a simple action wrapping to ActualState and sending to channel: ${ActualState(action, actualState)}")
//            channel.onNext(ActualState(action, actualState))
//        }
//
//    }
//  })
//
//  val parent = parentFactory.subscribe { (prevState, action) =>
//    println(s"[$name] a new state was requested for $prevState and $action")
//
//    action match {
//      case ActualState(_, actualState: T) if(actualState.uid == initialState.uid) =>
//        println(s"[$name] action is ActualState so we just unwrap it => $actualState")
//        actualState
//      case e: ActionWrapper =>
//        val newState = update(e.action, prevState, actions)
//        println(s"[$name] action is ActionWrapper so unwrap it and call update => $newState")
//        actions.onNext(Step(e.action, newState))
//        newState
//      case e: Action =>
//        val newState = update(e, prevState, actions)
//        println(s"[$name] action is a simple action so we call update => $newState")
//        newState
//      case _ =>
//        println(s"[$name] something went wrong, we return the prevState: $prevState")
//        prevState
//    }
//  }
//
//  def init() = render.onNext(initialState)
//
//  changes.distinctUntilChanged.subscribe(subscriber)
//
//}
//
//case class StateChange[T](action: Action, state: T) extends ActionWrapper
//
//object Counter extends GenericModule[CounterModel, CounterDispatcher] {
//  import scala.concurrent.ExecutionContext.Implicits.global
//
//  case class Increment(id: String) extends LocalAction
//
//  case class Decrement(id: String) extends Action
//
//  case class Close(id: String) extends Action
//
//  def upd(action: Action, model: CounterModel, actionsChannel: Observer[Action]): CounterModel = action match {
//    case Close(model.uid) =>
//      model
//    case Increment(model.uid) =>
//      Future {
//        println("Working....")
//        Thread.sleep(3000)
//        println("Ready....")
//        model.copy(value = model.value + 1)
//      } map { newState =>
//        actionsChannel.onNext(StateChange(action, newState))
//      }
//      model.copy(buttonEnabled = false)
//    case Decrement(model.uid) =>
////      Future {
////        Thread.sleep(2000)
////        model.copy(value = model.value - 1)
////      } map { newState =>
////        actionsChannel.onNext(ActualState(Nop, newState))
////      }
//
//      model.copy(value = model.value - 1)
//
//    case _ => model
//  }
//}
//
//
//class Counter extends GenericJavaFXModule[Counter.type] {
//  @FXML private var _lblCounter: Label = _
//  @FXML private var _btnIncrement: Button = _
//  @FXML private var _btnDecrement: Button = _
//  @FXML private var _btnClose: Button = _
//
//  lazy val lblCounter = _lblCounter
//  lazy val btnIncrement = _btnIncrement
//  lazy val btnDecrement = _btnDecrement
//  lazy val btnClose = _btnClose
//
//  def subscriber(changes: Observable[CounterModel]): (Observer[Action]) => Subscriber[CounterModel] = { actions =>
//    new Subscriber[CounterModel] {
//      override def onNext(model: CounterModel): Unit = {
//
//        btnIncrement.setDisable(!model.buttonEnabled)
//        btnDecrement.setDisable(!model.buttonEnabled)
//
//        lblCounter.setText(s"${model.uid} - ${model.value.toString}" )
//        btnIncrement.setOnAction { () => actions.onNext(Increment(model.uid)) }
//        btnDecrement.setOnAction { () => actions.onNext(Decrement(model.uid)) }
//        btnClose.setOnAction { () => actions.onNext(Close(model.uid)) }
//      }
//
//      override def onError(error: Throwable): Unit = super.onError(error)
//
//      override def onCompleted(): Unit = super.onCompleted()
//    }
//  }
//
//  override def dispatch(parentFactory: DispatcherFactory[CounterModel, Action], actions: Observer[Action], changes: Observable[CounterModel], initialState: CounterModel): CounterDispatcher = {
//    CounterDispatcher(parentFactory, actions, changes, subscriber(changes), initialState)
//  }
//}
