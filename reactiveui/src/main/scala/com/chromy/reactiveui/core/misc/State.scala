package com.chromy.reactiveui.core.misc

/**
 * Created by cry on 2015.05.20..
 */
trait State[S, +A] {
  def run: S => (S, A)

  def map[B](f: A => B): State[S, B] = State {s =>
    val (s1, a) = run(s)
    (s1, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {s =>
    val (s1, a) = run(s)
    f(a).run(s1)
  }
}

object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    def run = f
  }
}