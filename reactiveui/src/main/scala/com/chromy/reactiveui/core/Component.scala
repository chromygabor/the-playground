package com.chromy.reactiveui.core

import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observer, Scheduler => ScalaScheduler, Subscriber}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

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

  protected[core] def context: Context[ModelType]
}

trait Updater[+M <: BaseModel] extends PartialFunction[Action, M]

case object Bypass extends Updater[Nothing] {
  override def isDefinedAt(x: Action): Boolean = false

  override def apply(v1: Action) = ???
}

case class Simple[M <: BaseModel](f: PartialFunction[Action, M]) extends Updater[M] {
  override def isDefinedAt(action: Action): Boolean = f.isDefinedAt(action)

  override def apply(action: Action): M = f(action)
}

trait Component[M <: BaseModel] extends BaseComponent {
  type ModelType = M

  protected def contextMapper: ContextMapper[ModelType]

  protected def initialState: ModelType

  protected def update(model: ModelType): Updater[ModelType] = Bypass

  protected case class Defer[A](result: A, update: (A, ModelType) => ModelType, uid: Uid = initialState.uid) extends Action

  private[this] lazy val _changes = BehaviorSubject[ModelType](initialState)

  final protected lazy val name = s"${this.getClass.getSimpleName}(${initialState.uid})"
  println(s"[$name] created with $initialState")

  final private[this] val subscriber: (Action, ModelType, ModelType) => ModelType = { (action, _, prevState) =>
    //println(s"[$name] a new state was requested for $prevState and $action")
    action match {
      case d: Defer[_] if d.uid == initialState.uid =>
        println(s"[$name] action is Defer so we run its update...")
        val actualState = d.update(d.result, prevState)
        println(s"[$name] update result is: $actualState")
        actualState
      case e: Action =>
        val upd = update(prevState)
        val newState = if (upd.isDefinedAt(e)) {
          println(s"[$name] update is defined at action so we call it...")
          val actualState = upd(e)
          println(s"[$name] update result is: $actualState")
          actualState
        } else {
          prevState
        }
        newState
      case _ =>
        println(s"[$name] something went wrong, we return the prevState: $prevState")
        prevState
    }
  }

  final protected[core] val context = contextMapper(subscriber)

  context.changes.distinctUntilChanged.subscribe(
  { change =>
    println(s"[$name] new change from parent, rendering: $change")
    _changes.onNext(change)
  }, { error => _changes.onError(error) }, { () => _changes.onCompleted() }
  )

  final implicit val chainExecutionContext: ExecutionContext = context.backgroundExecutor
  final val channel: Observer[Action] = context.channel

  final def subscribe(subscriber: Subscriber[ModelType]) = {
    _changes.distinctUntilChanged.subscribe(subscriber)
  }


  final protected def fut[A](f: => A)(update: (Try[A], ModelType) => ModelType) = {
    val future = Future[A] {
      f
    }

    future.onComplete {
      case e => context.channel.onNext(Defer(e, update))
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
