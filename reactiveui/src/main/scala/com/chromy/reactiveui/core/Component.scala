package com.chromy.reactiveui.core

import com.chromy.reactiveui.myjavafx._
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Subscriber, Observer, Subject}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.07.11..
 */

trait Component {
  type ModelType <: Model[_ <: Component]

  def router: Router[ModelType]
}

trait Model[C <: Component] {
  def uid: Uid
}

trait BaseComponent[M <: Model[_ <: Component]] extends Component {
  type ModelType = M

  protected def routerMapper: RouterMapper[ModelType]

  protected def initialState: ModelType

  def update: (Action, ModelType, Observer[Action]) => ModelType

  /** Private parts **/
  case class ActualState(action: Action, state: ModelType) extends ActionWrapper

  case class Step(action: Action, state: ModelType) extends Action

  private val _channel = Subject[Action]
  private lazy val _changes = BehaviorSubject[ModelType]

  private lazy val name = s"${this.getClass.getSimpleName}(${initialState.uid})"
  println(s"[$name] created with $initialState")

  def subscriber: (Action, ModelType, ModelType) => ModelType = { (action, _, prevState) =>
    println(s"[$name] a new state was requested for $prevState and $action")

    action match {
      case ActualState(_, actualState) if (actualState.uid == initialState.uid) =>
        println(s"[$name] action is ActualState so we just unwrap it => $actualState")
        actualState
      case e: ActionWrapper =>
        val newState = update(e.action, prevState, _channel)
        println(s"[$name] action is ActionWrapper so unwrap it and call update => $newState")
        _channel.onNext(Step(e.action, newState))
        newState
      case e: Action =>
        val newState = update(e, prevState, _channel)
        println(s"[$name] action is a simple action so we call update => $newState")
        _channel.onNext(Step(e, newState))
        newState
      case _ =>
        println(s"[$name] something went wrong, we return the prevState: $prevState")
        prevState
    }
  }

  val router = routerMapper(subscriber)

  router.changes.distinctUntilChanged.subscribe(
  { change =>
    println(s"[$name] new change from parent: $change")
    _changes.onNext(change)
  }, { error => _changes.onError(error) }, { () => _changes.onCompleted() }
  )
  //_channel.subscribe({action => println(action)})
  private val stream = _channel.scan((initialState, initialState, Nop.asInstanceOf[Action])) { case ((beforePrevState, prevState, prevAction), action) =>
    Try[ModelType] {
      action match {
        case Step(action, newState) =>
          newState
        case StateChange(actionToWrap, actualState: ModelType) =>
          println(s"=================== [$name] action received  $action =================")
          println(s"[$name] action is ActualState $actualState")
          actualState
        case action: ActionWrapper =>
          println(s"[$name] action is already a WrapperAction so return the previous state")
          prevState
        case action =>
          println(s"=================== [$name] action received  $action =================")
          val newState = update(action, prevState, _channel)
          println(s"[$name] action triggered a new state: $prevState => $newState")
          newState
      }
    } match {
      case Success(newState) => (prevState, newState, action)
      case Failure(error) =>
        error.printStackTrace()
        (beforePrevState, prevState, prevAction)
    }
  }

  stream.drop(1).distinctUntilChanged.subscribe({ input =>
    input match {
      case (prevState, actualState, action) =>
        action match {
          case Step(_, _) =>
            println(s"[$name] action is a step so nothing to do: $action")
          case e: LocalAction =>
            println(s"[$name] action is LocalAction triggering render: $actualState")
            _changes.onNext(actualState)
          case action@StateChange(a, s: ModelType) =>
            val wrap = ActualState(a, s)
            println(s"[$name] action is StateChange sending to channel as a $wrap")
            router.channel.onNext(wrap)
          case action: ActionWrapper =>
            println(s"[$name] action is already a WrapperAction so just send up to channel: ${action}")
            router.channel.onNext(action)
          case action =>
            println(s"[$name] action is a simple action wrapping to ActualState and sending to channel: ${ActualState(action, actualState)}")
            router.channel.onNext(ActualState(action, actualState))
        }
    }
  })
  _changes.onNext(initialState)
  //_changes.subscribe({item => println(s"changes: $item")})

  def subscribe(subscriber: Subscriber[ModelType]) = {
    _changes.subscribe(subscriber)
  }
  val channel: Observer[Action] = _channel
}







