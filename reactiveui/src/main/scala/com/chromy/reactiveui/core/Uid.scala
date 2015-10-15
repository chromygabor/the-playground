package com.chromy.reactiveui.core

import java.util.concurrent.atomic.AtomicInteger

/**
 * Created by cry on 2015.08.04..
 */
object Uid {
  val _nextUid = new AtomicInteger(0)
  private[core] def next(): Int = _nextUid.getAndIncrement()
}

case class Uid(uid: Int = Uid.next())
