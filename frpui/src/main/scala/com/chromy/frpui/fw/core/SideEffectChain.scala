package com.chromy.frpui.fw.core

import java.util.concurrent.atomic.AtomicReference

import scala.collection.mutable.{Map => MMap, WeakHashMap => WMap}


trait SideEffectChain[T] {
  private[this] val _subscribersLock: Object = new Object()
  private[this] val _subscribers: WMap[(T => SideEffect), Int] = WMap()
  protected val _lastItem: AtomicReference[Option[T]] = new AtomicReference(None)

  private[SideEffectChain] def subscribers: List[(T => SideEffect)] = {
    var newSubscribers: List[T => SideEffect] = null
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

      override val update: T => SideEffect = { in =>
        if (pred(in)) {
          _lastItem.set(Some(in))
          subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
            val s = { () => subscriber(in) }
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
      private val parent = myParent
      override protected val _lastItem = new AtomicReference[Option[T]](parent._lastItem.get())

      override val update: T => SideEffect = { in =>
        if (_lastItem.getAndSet(Some(in)) != in) {
          subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
            val s = { () => subscriber(in) }
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
      override protected val _lastItem = new AtomicReference[Option[B]](parent._lastItem.get.map(f))

      private val updater: T => SideEffect = { in =>
        update(f(in))
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
     
      private val updater: T => SideEffect = { in =>
        val l = in.asInstanceOf[Seq[B]]
        if (l.isDefinedAt(index)) {
          update(l(index))
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

      private val updater: T => SideEffect = { in =>
        val l = in.asInstanceOf[Map[K, V]]
        if (l.isDefinedAt(key)) {
          update(l(key))
        } else {
          SideEffect()
        }
      }
      parent.subscribe(updater)
    }
  }

  val update: T => SideEffect = { in =>
    _lastItem.set(Some(in))
    subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
      val s = { () => subscriber(in) }
      executables.append(s)
    }
  }

  def subscribe(subscriber: (T) => SideEffect): Unit = {
    _subscribersLock.synchronized {
      _subscribers.update(subscriber, _subscribers.size)
    }
  }

}

object SideEffectChain {
  def apply[T](): SideEffectChain[T] = new SideEffectChain[T] { }
}
