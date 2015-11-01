package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */
object EventHandler {
  def apply[B](iHandle: PartialFunction[Event, B]) = new EventHandler[B] { override val handle: PartialFunction[Event, B] = iHandle }
  def apply() = new EventHandler[Nothing] {
    override val handle: PartialFunction[Event, Nothing] = new PartialFunction[Event, Nothing] {
      override def isDefinedAt(x: Event): Boolean = false

      override def apply(v1: Event): Nothing = ???
    }
  }
}

trait EventHandler[+B] {
  val handle: PartialFunction[Event, B]
}