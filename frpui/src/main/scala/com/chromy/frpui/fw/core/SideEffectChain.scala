package com.chromy.frpui.fw.core

import java.util.concurrent.atomic.AtomicReference

import scala.collection.mutable.{Map => MMap, WeakHashMap => WMap}


trait SideEffectChain[T] {
  private[this] val _subscribersLock: Object = new Object()
  private[this] val _subscribers: WMap[((T, Context) => SideEffect), Int] = WMap()
  protected val _lastItem: AtomicReference[Option[T]] = new AtomicReference(None)

  private[SideEffectChain] def subscribers: List[((T, Context) => SideEffect)] = {
    var newSubscribers: List[(T, Context) => SideEffect] = null
    _subscribersLock.synchronized {
      newSubscribers = _subscribers.toList.sortBy(_._2).map {
        _._1
      }
    }
    newSubscribers
  }

  def lastItem: Option[T] = _lastItem.get

  def filter(pred: T => Boolean): SideEffectChain[T] = {
    val myParent = this
    new SideEffectChain[T] {
      private val parent = myParent
      override protected val _lastItem = new AtomicReference[Option[T]](parent._lastItem.get.filter(pred))

      override val update: (T, Context) => SideEffect = { case (in, context) =>
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

  def distinctUntilChanged: SideEffectChain[T] = {
    val myParent = this
    new SideEffectChain[T] {
      private val parent = myParent
      override protected val _lastItem = new AtomicReference[Option[T]](parent._lastItem.get())

      override val update: (T, Context) => SideEffect = { case (in, context) =>
        if (_lastItem.getAndSet(Some(in)) != in) {
          subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
            executables.++(subscriber(in, context))
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
      override protected val _lastItem = new AtomicReference[Option[B]](parent._lastItem.get.map(f))

      private val updater: (T, Context) => SideEffect = { case (in, context) =>
        update(f(in), context)
      }
      parent.subscribe(updater)
    }
  }

  def indexOption[B](index: Int)(implicit ev: T <:< Seq[B]): SideEffectChain[B] = {
    val myParent = this

    new SideEffectChain[B] {
      private val parent = myParent
      
      override protected val _lastItem = {
        val parentSeq = parent._lastItem.get.asInstanceOf[Option[Seq[B]]]
        new AtomicReference(parentSeq.flatMap(_.lift(index)))
      }
     
      private val updater: (T, Context) => SideEffect = { case (in, context) =>
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

  def keyOption[K, V](key: K)(implicit ev: T <:< Map[K, V]): SideEffectChain[V] = {
    val myParent = this
    
    new SideEffectChain[V] {
      private val parent = myParent
      
      override protected val _lastItem = {
        val parentMap = parent._lastItem.get.asInstanceOf[Option[Map[K, V]]]
        new AtomicReference(parentMap.flatMap(in => in.get(key)))
      }

      private val updater: (T, Context) => SideEffect = { case (in, context) =>
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

  val update: (T, Context) => SideEffect = { case (in, context) =>
    _lastItem.set(Some(in))
    subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
      executables.++(subscriber(in, context))
    }
  }

  def subscribe(subscriber: (T, Context) => SideEffect): Unit = {
    _subscribersLock.synchronized {
      _subscribers.update(subscriber, _subscribers.size)
    }
  }

}

object SideEffectChain {
  def apply[T](): SideEffectChain[T] = new SideEffectChain[T] { }
}
