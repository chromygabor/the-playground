package com.chromy.reactiveui.core

/**
 * Created by cry on 2015.08.04..
 */
object Uid {
  var _nextUid = 0
  private[core] def next(): Int = {
    val thisUid = _nextUid
    _nextUid += 1
    thisUid
  }
}

case class Uid(uid: Int = Uid.next())
