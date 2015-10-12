package com.chromy.reactiveui.core.misc

import java.util.concurrent.atomic.AtomicReference

import scala.collection.mutable.{WeakHashMap => WMap, Map => MMap}
import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.09.22..
 */

trait Executable {
  def run(): Unit

  def errors: List[Throwable]

  def append(subscriber: () => Executable): Executable = {
    Try {
      subscriber()
    } match {
      case Success(executable) => Executable({ run(); executable.run()}, errors)
      case Failure(t) => Executable(run(), t :: errors)
    }
  }
}

object Executable {
  def apply(): Executable = apply( {} )

  def apply(f: => Unit): Executable = new Executable {
    override def run(): Unit = f

    override val errors = List.empty[Throwable]
  }

  def apply(f: => Unit, iErrors: List[Throwable]): Executable = new Executable {
    override def run(): Unit = f

    override val errors = iErrors
  }

}

trait BehaviorSideChain[T] extends SideChain[T] {
  def subscribeAndApply(subscriber: (T) => Executable): Executable
}

trait SideChain[T] {
  private[this] val _subscribersLock: Object = new Object()
  private[this] val _subscribers: MMap[(T => Executable), Int] = WMap()

  private[SideChain] def subscribers: List[(T => Executable)] = {
    var newSubscribers: List[T => Executable] = null
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

      override val update: T => Executable = { in =>
        if (lastItem.getAndSet(in) != in) {
          val allSubscribers = subscribers.map(_.apply(in))
          Executable {
            allSubscribers.foreach(_.run())
          }
        } else Executable {}
      }
      parent.subscribe(update)

      override def subscribeAndApply(subscriber: (T) => Executable): Executable = {
        subscribe(subscriber)
        subscriber.apply(lastItem.get)
      }
    }
  }

  def filter(pred: T => Boolean): SideChain[T] = {
    val myParent = this
    new SideChain[T] {
      private val parent = myParent
      override val update: T => Executable = { in =>
        if (pred(in)) {
          subscribers.foldLeft(Executable()) { case (executables, subscriber) =>
            val s ={ () => subscriber(in) }
            executables.append(s)
          }
        } else Executable()
      }
      parent.subscribe(update)
    }
  }

  def distinctUntilChanged: SideChain[T] = {
    val myParent = this
    new SideChain[T] {
      private val lastItem = new AtomicReference[T]()
      private val parent = myParent

      override val update: T => Executable = { in =>
        if (lastItem.getAndSet(in) != in) {
          subscribers.foldLeft(Executable()) { case (executables, subscriber) =>
            val s ={ () => subscriber(in) }
            executables.append(s)
          }
        } else Executable {}
      }
      parent.subscribe(update)
    }

  }

  def map[B](f: T => B): SideChain[B] = {
    val myParent = this
    new SideChain[B] {
      private val parent = myParent
      override val update: B => Executable = { in =>
        subscribers.foldLeft(Executable()) { case (executables, subscriber) =>
          val s ={ () => subscriber(in) }
          executables.append(s)
        }
      }

      private val updater: T => Executable = { in =>
        update(f(in))
      }
      parent.subscribe(updater)
    }
  }

  val update: T => Executable

  def subscribe(subscriber: (T) => Executable): Unit = {
    _subscribersLock.synchronized {
      _subscribers.update(subscriber, _subscribers.size)
    }
  }

}

object SideChain {
  def apply[T](): SideChain[T] = new SideChain[T] {
    override val update: T => Executable = { in =>
      subscribers.foldLeft(Executable()) { case (executables, subscriber) =>
        val s ={ () => subscriber(in) }
        executables.append(s)
      }
    }
  }
}
