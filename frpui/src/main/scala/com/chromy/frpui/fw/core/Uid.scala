package com.chromy.frpui.fw.core

import java.util.concurrent.atomic.AtomicInteger

import scala.util.Random

/**
 * Created by cry on 2015.08.04..
 */
object Uid {
  val _nextUid = new AtomicInteger(Random.nextInt(100000))
  private[core] def next(): String = _nextUid.getAndSet(Random.nextInt(100000)).toString
}

case class Uid(uid: String = Uid.next())
