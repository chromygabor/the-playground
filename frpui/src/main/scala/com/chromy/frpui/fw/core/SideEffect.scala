package com.chromy.frpui.fw.core

import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.11.01..
 */
object SideEffect {
  def apply(): SideEffect = apply( {} )

  def apply(f: => Unit): SideEffect = new SideEffect {
    override def run(): Unit = f

    override val errors = List.empty[Throwable]
  }

  def apply(f: => Unit, iErrors: List[Throwable]): SideEffect = new SideEffect {
    override def run(): Unit = f

    override val errors = iErrors
  }

}

trait SideEffect {
  def run(): Unit

  def errors: List[Throwable]

  def +(subscriber : => Unit): SideEffect = {
    Try {
      SideEffect(subscriber)
    } match {
      case Success(executable) => SideEffect({ run(); executable.run()}, errors)
      case Failure(t) => SideEffect(run(), t :: errors)
    }
  }

  def ++(subscriber : => SideEffect): SideEffect = {
    Try {
      subscriber
    } match {
      case Success(executable) => SideEffect({ run(); executable.run()}, errors)
      case Failure(t) => SideEffect(run(), t :: errors)
    }
  }
}