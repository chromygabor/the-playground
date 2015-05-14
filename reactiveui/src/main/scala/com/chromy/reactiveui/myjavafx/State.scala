package com.chromy.reactiveui.myjavafx

/**
 * Created by cry on 2015.05.10..
 */
trait State[S, A] {
  def run: S => (A, S)
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, t) = run(s)
      (f(a), t)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, t) = run(s)
      f(a) run t
    })
}

object State {
  def apply[S,A](pRun: S => (A, S)): State[S, A] = new State[S,A] {
    override def run = pRun
  }
}
