package com.chromy.frpui.fw.core

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference

/**
 * Created by cry on 2015.08.04..
 */
object Uid {
  val _nextUid = new AtomicReference(UUID.randomUUID().toString)
  private[core] def next(): String = _nextUid.getAndSet(UUID.randomUUID().toString)
}

case class Uid(uid: String = Uid.next()) {
  override def toString(): String = uid.take(6) 
}
