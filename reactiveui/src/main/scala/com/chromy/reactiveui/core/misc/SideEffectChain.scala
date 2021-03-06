package com.chromy.reactiveui.core.misc

import java.util.concurrent.atomic.AtomicReference

import scala.collection.mutable.{WeakHashMap => WMap, Map => MMap}
import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.09.22..
 */

trait SideEffect {
  def run(): Unit

  def errors: List[Throwable]

  def append(subscriber: () => SideEffect): SideEffect = {
    Try {
      subscriber()
    } match {
      case Success(executable) => SideEffect({ run(); executable.run()}, errors)
      case Failure(t) => SideEffect(run(), t :: errors)
    }
  }
}

object SideEffect {
  def apply(): SideEffect = apply( {} )

  def apply(f: => Unit): SideEffect = new SideEffect {
    override def run(): Unit = f

    override val errors = List.empty[Throwable]
  }

  def apply(f: => Unit, iErrors: List[Throwable]): SideEffect = new SideEffect {
    override def run(): Unit = f

    override val errors = iErrors
  }

}

trait BehaviorSideChain[T] extends SideEffectChain[T] {
  def subscribeAndApply(subscriber: (T) => SideEffect): SideEffect
}

trait SideEffectChain[T] {
  private[this] val _subscribersLock: Object = new Object()
  private[this] val _subscribers: MMap[(T => SideEffect), Int] = WMap()

  private[SideEffectChain] def subscribers: List[(T => SideEffect)] = {
    var newSubscribers: List[T => SideEffect] = null
    _subscribersLock.synchronized {
      newSubscribers = _subscribers.toList.sortBy(_._2).map {
        _._1
      }
    }
    newSubscribers
  }

  def asBehavior(initialState: T): BehaviorSideChain[T] = {
    val myParent = this
    new BehaviorSideChain[T] {
      private val parent = myParent
      private val lastItem = new AtomicReference[T](initialState)

      override val update: T => SideEffect = { in =>
        if (lastItem.getAndSet(in) != in) {
          val allSubscribers = subscribers.map(_.apply(in))
          SideEffect {
            allSubscribers.foreach(_.run())
          }
        } else SideEffect {}
      }
      parent.subscribe(update)

      override def subscribeAndApply(subscriber: (T) => SideEffect): SideEffect = {
        subscribe(subscriber)
        subscriber.apply(lastItem.get)
      }
    }
  }

  def filter(pred: T => Boolean): SideEffectChain[T] = {
    val myParent = this
    new SideEffectChain[T] {
      private val parent = myParent
      override val update: T => SideEffect = { in =>
        if (pred(in)) {
          subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
            val s ={ () => subscriber(in) }
            executables.append(s)
          }
        } else SideEffect()
      }
      parent.subscribe(update)
    }
  }

  def distinctUntilChanged: SideEffectChain[T] = {
    val myParent = this
    new SideEffectChain[T] {
      private val lastItem = new AtomicReference[T]()
      private val parent = myParent

      override val update: T => SideEffect = { in =>
        if (lastItem.getAndSet(in) != in) {
          subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
            val s ={ () => subscriber(in) }
            executables.append(s)
          }
        } else SideEffect {}
      }
      parent.subscribe(update)
    }

  }

  def map[B](f: T => B): SideEffectChain[B] = {
    val myParent = this
    new SideEffectChain[B] {
      private val parent = myParent
      override val update: B => SideEffect = { in =>
        subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
          val s ={ () => subscriber(in) }
          executables.append(s)
        }
      }

      private val updater: T => SideEffect = { in =>
        update(f(in))
      }
      parent.subscribe(updater)
    }
  }

  val update: T => SideEffect

  def subscribe(subscriber: (T) => SideEffect): Unit = {
    _subscribersLock.synchronized {
      _subscribers.update(subscriber, _subscribers.size)
    }
  }

}

object SideEffectChain {
  def apply[T](): SideEffectChain[T] = new SideEffectChain[T] {
    override val update: T => SideEffect = { in =>
      subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
        val s ={ () => subscriber(in) }
        executables.append(s)
      }
    }
  }
}
