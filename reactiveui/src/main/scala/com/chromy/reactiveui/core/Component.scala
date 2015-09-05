package com.chromy.reactiveui.core

import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observer, Scheduler => ScalaScheduler, Subject, Subscriber}
import rx.schedulers.Schedulers

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.07.11..
 */

trait BaseModel {
  type Component <: BaseComponent

  def uid: Uid
}

trait Model[C <: BaseComponent] extends BaseModel {
  type Component = C
}

trait BaseComponent {
  type ModelType

  def router: Router[ModelType]
}

trait Component[M <: BaseModel] extends BaseComponent {
  type ModelType = M

  protected def routerMapper: RouterMapper[ModelType]

  protected def initialState: ModelType

  protected def upd(model: ModelType): PartialFunction[Action, ModelType]

  /** Private parts **/
  case class ActualState(action: Action, state: ModelType) extends ActionWrapper

  case class Step(action: Action, state: ModelType) extends Action

  case class Defer[A](result: Try[A], update: (Try[A], ModelType) => ModelType, uid: Uid = initialState.uid) extends Action

  private[this] val _channel = Subject[Action]
  private[this] lazy val _changes = BehaviorSubject[ModelType](initialState)

  protected lazy val name = s"${this.getClass.getSimpleName}(${initialState.uid})"
  println(s"[$name] created with $initialState")

  val subscriber: (Action, ModelType, ModelType) => ModelType = { (action, _, prevState) =>
    println(s"[$name] a new state was requested for $prevState and $action")
    action match {
      case d: Defer[_] if d.uid == initialState.uid =>
        val actualState = d.update(d.result, prevState)
        println(s"[$name] action is Defer so we run its update it => $actualState")
        _channel.onNext(Step(d, actualState))
        actualState
      case ActualState(_, actualState) if (actualState.uid == initialState.uid) =>
        println(s"[$name] action is ActualState so we just unwrap it => $actualState")
        actualState
      case e: ActionWrapper =>
        val update = upd(prevState)
        val newState = if(update.isDefinedAt(e.action)) update(e.action) else prevState
        println(s"[$name] action is ActionWrapper so unwrap it and called update => $newState")
        _channel.onNext(Step(e.action, newState))
        newState
      case e: Action =>
        val update = upd(prevState)
        val newState = if(update.isDefinedAt(e)) update(e) else prevState
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

  lazy val chainScheduler = new ScalaScheduler {
    val asJavaScheduler = Schedulers.from(router.chainExecutor)
  }

  lazy val changesScheduler = new ScalaScheduler {
    val asJavaScheduler = Schedulers.from(router.changesExecutor)
  }

  implicit val chainExecutionContext:ExecutionContext = ExecutionContext.fromExecutor(router.chainExecutor)

  private[this] val stream = _channel.observeOn(chainScheduler).scan((initialState, initialState, Nop.asInstanceOf[Action])) { case ((beforePrevState, prevState, prevAction), action) =>
    Try[ModelType] {
      action match {
        case Step(action, newState) =>
          newState
        case StateChange(actionToWrap, actualState: ModelType) =>
          println(s"=================== [$name] StateChange action received  $action =================")
          println(s"[$name] action is ActualState $actualState")
          actualState
        case action: ActionWrapper =>
          println(s"[$name] action is already a WrapperAction so return the previous state")
          prevState
        case action =>
          println(s"=================== [$name] action received  $action =================")
          val update = upd(prevState)
          val newState = if(update.isDefinedAt(action)) update(action) else prevState
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

  def subscribe(subscriber: Subscriber[ModelType]) = {
    _changes.observeOn(changesScheduler).subscribe(subscriber)
  }

  implicit val channel: Observer[Action] = _channel

  def fut[A](f: => A)(update: (Try[A], ModelType) => ModelType) = {
    val future = Future[A] {
      f
    }

    future.onComplete {
      case e => router.channel.onNext(Defer(e, update))
    }
  }
}

object Component {

  import reflect.runtime.{currentMirror => mirror}
  import scala.reflect.runtime.universe._

  implicit class ComponentUtil[A <: BaseComponent : Manifest](component: A) {
    def modelType: Type = {
      val m = typeOf[A].member("M": TypeName)
      val tpe = typeOf[A]
      m.asType.toTypeIn(tpe)
    }
  }

}
