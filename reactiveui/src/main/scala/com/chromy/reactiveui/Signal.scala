package com.chromy.reactiveui

import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observer, Observable, Subject}

/**
 * Created by chrogab on 2015.04.24..
 */
object Signal {
  trait Mailbox[T] {
    val address: Address[T]
    val signal: Signal[T]
  }
  //  trait Address[T] extends Observer[T] {
  //    def apply(value: T) = onNext(value)
  //  }

  type Address[T] = Observer[T]
  type Signal[T] = Observable[T]

  def map[A, B](f: A => B) (in: Signal[A]): Signal[B] = in.map(f)

  def mailbox[T]: Mailbox[T] = new Mailbox[T] {
    val subject = BehaviorSubject[T]()
    val address: Address[T] = subject.asInstanceOf[Address[T]]
    val signal: Signal[T]  = subject.asInstanceOf[Signal[T]]
  }

  def forwardTo[A,B](address: Signal.Address[A])(f: B => A): Signal.Address[B] = {
    val mb = mailbox[B]

    mb.signal.subscribe { in =>
      address.onNext(f(in))
    }

    mb.address
  }
}
