package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx.Action
import monocle._

import scala.collection.mutable

/**
 * Created by cry on 2015.07.04..
 */
trait UpdateChain[T] {

  //STM should be used
  private[this] val _subscribers: mutable.WeakHashMap[((Action, T, T) => T), Int] = mutable.WeakHashMap()

  private[this] def subscribers: List[(Action, T, T) => T] = {
    _subscribers.toList.sortBy(_._2).map {_._1}
  }


  def fromLens[B](lens: Lens[T, B]): UpdateChain[B] = {
    val parent = UpdateChain.this
    new UpdateChain[B] {

      private def updater : (Action, T, T) => T = { (action, originalModel, model) =>
        val newModel = this.innerUpdate(action, lens.get(originalModel), lens.get(model))
        val res = lens.set(newModel)
        res(model)
      }

      parent.subscribe(updater)
    }
  }

  def subscribe(subscriber: (Action, T, T) => T): Unit = {
    _subscribers.update(subscriber, _subscribers.size)
  }

  def subscribe(subscriber: (Action, T) => T): Unit = {
    def realSubscriber: (Action, T, T) => T = { (action, originalModel, model) =>
      subscriber(action, model)
    }
    subscribe(realSubscriber)
  }

  def update: (Action, T) => T = { (action, model) =>
    innerUpdate(action, model, model)
  }

  private[reactiveui] def innerUpdate:  (Action, T, T) => T = { (action, originalModel, model) =>
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

