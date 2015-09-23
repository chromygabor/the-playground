package com.chromy.reactiveui.core

/**
 * Created by cry on 2015.09.13..
 */
trait Updater[+M <: ComponentModel] extends PartialFunction[Action, M]

case object Bypass extends Updater[Nothing] {
  override def isDefinedAt(x: Action): Boolean = false

  override def apply(v1: Action) = ???
}

case class Simple[M <: ComponentModel](f: PartialFunction[Action, M]) extends Updater[M] {
  override def isDefinedAt(action: Action): Boolean = f.isDefinedAt(action)

  override def apply(action: Action): M = f(action)
}

