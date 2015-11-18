package com.chromy.frpui.fw.core

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.Renderer

import scala.collection.mutable.{Map => MMap, WeakHashMap => WMap}


trait SideEffectChain[M, C] {
  private[this] val _subscribersLock: Object = new Object()
  private[this] val _subscribers: WMap[((M, C) => SideEffect), Int] = WMap()
  protected val _lastItem: AtomicReference[Option[M]] = new AtomicReference(None)

  private[SideEffectChain] def subscribers: List[((M, C) => SideEffect)] = {
    var newSubscribers: List[(M, C) => SideEffect] = null
    _subscribersLock.synchronized {
      newSubscribers = _subscribers.toList.sortBy(_._2).map {
        _._1
      }
    }
    newSubscribers
  }

  def lastItem: Option[M] = _lastItem.get

  def filter(pred: M => Boolean): SideEffectChain[M, C] = {
    val myParent = this
    new SideEffectChain[M, C] {
      private val parent = myParent
      override protected val _lastItem = new AtomicReference[Option[M]](parent._lastItem.get.filter(pred))

      override val update: (M, C) => SideEffect = { case (in, context) =>
        if (pred(in)) {
          _lastItem.set(Some(in))
          subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
            executables.++(subscriber(in, context))
          }
        } else SideEffect()
      }
      parent.subscribe(update)
    }
  }

  def distinctUntilChanged: SideEffectChain[M, C] = {
    val myParent = this
    new SideEffectChain[M, C] {
      private val parent = myParent
      override protected val _lastItem = new AtomicReference[Option[M]](parent._lastItem.get())

      override val update: (M, C) => SideEffect = { case (in, context) =>
        if (_lastItem.getAndSet(Some(in)) != in) {
          subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
            executables.++(subscriber(in, context))
          }
        } else SideEffect {}
      }
      parent.subscribe(update)
    }
  }

  def map[B](f: M => B): SideEffectChain[B, C] = {
    val myParent = this
    new SideEffectChain[B, C] {
      private val parent = myParent
      override protected val _lastItem = new AtomicReference[Option[B]](parent._lastItem.get.map(f))

      private val updater: (M, C) => SideEffect = { case (in, context) =>
        update(f(in), context)
      }
      parent.subscribe(updater)
    }
  }

  def indexOption[B](index: Int)(implicit ev: M <:< Seq[B]): SideEffectChain[B, C] = {
    val myParent = this

    new SideEffectChain[B, C] {
      private val parent = myParent
      
      override protected val _lastItem = {
        val parentSeq = parent._lastItem.get.asInstanceOf[Option[Seq[B]]]
        new AtomicReference(parentSeq.flatMap(_.lift(index)))
      }
     
      private val updater: (M, C) => SideEffect = { case (in, context) =>
        val l = in.asInstanceOf[Seq[B]]
        if (l.isDefinedAt(index)) {
          update(l(index), context)
        } else {
          SideEffect()
        }
      }
      parent.subscribe(updater)
    }
  }

  def keyOption[K, V](key: K)(implicit ev: M <:< Map[K, V]): SideEffectChain[V, C] = {
    val myParent = this
    
    new SideEffectChain[V, C] {
      private val parent = myParent
      
      override protected val _lastItem = {
        val parentMap = parent._lastItem.get.asInstanceOf[Option[Map[K, V]]]
        new AtomicReference(parentMap.flatMap(in => in.get(key)))
      }

      private val updater: (M, C) => SideEffect = { case (in, context) =>
        val l = in.asInstanceOf[Map[K, V]]
        if (l.isDefinedAt(key)) {
          update(l(key), context)
        } else {
          SideEffect()
        }
      }
      parent.subscribe(updater)
    }
  }

  val update: (M, C) => SideEffect = { case (in, context) =>
    _lastItem.set(Some(in))
    subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
      executables.++(subscriber(in, context))
    }
  }

  def subscribe(subscriber: (M, C) => SideEffect): Unit = {
    _subscribersLock.synchronized {
      _subscribers.update(subscriber, _subscribers.size)
    }
  }

}

object SideEffectChain {
  def apply[T, C](): SideEffectChain[T, C] = new SideEffectChain[T, C] { }
}
