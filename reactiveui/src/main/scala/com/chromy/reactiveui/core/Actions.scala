package com.chromy.reactiveui.core

/**
 * Created by cry on 2015.08.04..
 */

trait Action

trait LocalAction extends Action

trait ActionWrapper extends Action {
  val action: Action
}

case class StateChange[T](action: Action, state: T) extends ActionWrapper

case object Nop extends Action