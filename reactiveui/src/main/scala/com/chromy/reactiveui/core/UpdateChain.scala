package com.chromy.reactiveui.core

import java.util.concurrent.atomic.AtomicReference

import monocle._

import scala.collection.mutable.{HashMap => MMap, WeakHashMap}

/**
 * Created by cry on 2015.07.04..
 */
trait UpdateChain[T] {

  private[this] val _subscribersLock: Object = new Object()
  private[this] val _subscribers: WeakHashMap[((Action, T, T) => T), Int] = WeakHashMap()

  private[this] def subscribers: List[(Action, T, T) => T] = {
    var newSubscribers:List[((Action, T, T) => T)]  = null
    _subscribersLock.synchronized {
      newSubscribers = _subscribers.toList.sortBy(_._2).map {
        _._1
      }
    }
    newSubscribers
  }


  def map[B](lens: Lens[T, B]): UpdateChain[B] = {
    val parent = UpdateChain.this
    new UpdateChain[B] {

      private val updater : (Action, T, T) => T = { (action, originalModel, model) =>
        val newModel = this.innerUpdate(action, lens.get(originalModel), lens.get(model))
        val res = lens.set(newModel)
        res(model)
      }

      parent.subscribe(updater)
    }
  }

  def subscribe(subscriber: (Action, T, T) => T): Unit = {
    _subscribersLock.synchronized {
      _subscribers.update(subscriber, _subscribers.size)
    }
  }

  val update: (Action, T) => T = { (action, model) =>
    innerUpdate(action, model, model)
  }

  private[reactiveui] val innerUpdate:  (Action, T, T) => T = { (action, originalModel, model) =>
    subscribers match {
      case Nil => model
      case h :: Nil => h(action, originalModel, model)
      case h :: t =>
        t.foldLeft(h(action, originalModel, model)) { (accu, act) =>
          act(action, originalModel, accu)
        }
    }

  }
}

object UpdateChain {
  def apply[T](): UpdateChain[T] = new UpdateChain[T] {}
}

