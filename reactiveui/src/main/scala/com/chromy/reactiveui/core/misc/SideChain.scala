package com.chromy.reactiveui.core.misc

import java.util.concurrent.atomic.AtomicReference

import scala.collection.mutable.{WeakHashMap => WMap}

/**
 * Created by cry on 2015.09.22..
 */

trait Executable {
  def run(): Unit
  def error(): Unit = {}
}
object Executable {
  def apply(f: => Unit) = new Executable {
    override def run(): Unit = f
  }

  def apply(success: => Unit, unsuccess: => Unit) = new Executable {
    override def run(): Unit = success
    override def error(): Unit = unsuccess
  }
}

trait SideChain[T] {
  private[this] val _subscribersLock: Object = new Object()
  private[this] val _subscribers: WMap[(T => Executable), Int] = WMap()

  private[SideChain] def subscribers: List[(T => Executable)] = {
    var newSubscribers:List[T => Executable]  = null
    _subscribersLock.synchronized {
      newSubscribers = _subscribers.toList.sortBy(_._2).map {
        _._1
      }
    }
    newSubscribers
  }

  def filter(pred: T => Boolean): SideChain[T] = {
    val parent = this
    new SideChain[T] {
      override val update: T => Executable = { in =>
        if(pred(in)) {
          val allSubscribers = subscribers.map(_.apply(in))
          Executable {
            allSubscribers.foreach(_.run())
          }
        } else Executable{}
      }
      parent.subscribe(update)
    }
  }

  def distinctUntilChanged: SideChain[T] = {
    val parent = this
    new SideChain[T] {
      val lastItem = new AtomicReference[T]()

      override val update: T => Executable = { in =>
        if(lastItem.getAndSet(in) != in) {
          val allSubscribers = subscribers.map(_.apply(in))
          Executable {
            allSubscribers.foreach(_.run())
          }
        } else Executable{}
      }
      parent.subscribe(update)
    }

  }

  def map[B](f: T => B): SideChain[B] = {
    val parent = this
    new SideChain[B] {
      override val update: B => Executable = { in =>
        val allSubscribers = subscribers.map(_.apply(in))
        Executable {
          allSubscribers.foreach(_.run())
        }
      }

      private val updater: T => Executable = { in =>
        update(f(in))
      }
      parent.subscribe(updater)
    }
  }

//  def to[B](lens: Lens[T,B]): SideChain[B] = {
//    val parent = SideChain.this
//    new SideChain[B] {
//      private val updater: T => () => Unit = { input =>
//        val newInput = lens.get(input)
//        this.update(newInput)
//      }
//      parent.subscribe(updater)
//    }
//  }

  val update: T => Executable

  def subscribe(subscriber: (T) => Executable): Unit = {
    _subscribersLock.synchronized {
      _subscribers.update(subscriber, _subscribers.size)
    }
  }

}

object SideChain {
  def apply[T]: SideChain[T] = new SideChain[T] {
    override val update: T => Executable = { in =>
      val allSubscribers = subscribers.map ( _.apply(in) )
      Executable {
        allSubscribers.foreach(_.run())
      }
    }
  }

  def behavior[T]: SideChain[T] = new SideChain[T] {
    override val update: T => Executable = { in =>
      val allSubscribers = subscribers.map ( _.apply(in) )
      Executable {
        allSubscribers.foreach(_.run())
      }
    }
  }

}
