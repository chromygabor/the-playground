package com.chromy.reactiveui

import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import com.chromy.reactiveui.myjavafx.State
import monocle.Lens
import monocle.macros.GenLens
import monocle.macros.internal.MacroImpl

/**
 * Created by cry on 2015.05.30..
 */
trait Dispatcher[S, A] {

  var _updates: List[(A) => State[S, A]] = Nil
  def update: A => State[S, A]

  def fromLens[B](theLens: Lens[S, B]): DispatcherFactory[B,A] = {
    lens(theLens.get, theLens.set)
  }

  def factory = {
    lens({input: S => input}, {input1: S => _ => input1})
  }

  def lens[B](iGet: S => B, iSet: (B) => S => S): DispatcherFactory[B, A] = new DispatcherFactory[B, A]{

    def subscribe(subscriber: (B, A) => B): Dispatcher[B, A] = {
      val dispatcher = new Dispatcher[B, A] {
        def iUpdate: (A) => State[B, A] = { op => State[B, A] { model =>
          subscriber(model, op) -> op
        }
        }
        def update: A => State[B, A] = { op =>
          _updates.foldLeft(iUpdate(op)) { (accu, update) =>
            val r2 = for {
              _ <- accu
              _ <- update(op)
            } yield (op)
            r2
          }
        }
      }

      def updater: (A) => State[S, A] = { op =>
        State[S, A] { model =>
          val (res, _) = dispatcher.update(op).run(iGet(model))
          iSet(res)(model) -> op
        }
      }
      _updates = updater :: _updates

      dispatcher
    }

    override def apply(iUpdate: (A) => State[B, A]): Dispatcher[B, A] = {
      val dispatcher = new Dispatcher[B, A] {
        def update: A => State[B, A] = { op =>
          _updates.foldLeft(iUpdate(op)) { (accu, update) =>
            val r2 = for {
              _ <- accu
              _ <- update(op)
            } yield (op)
            r2
          }
        }
      }

      def updater: (A) => State[S, A] = { op =>
        State[S, A] { model =>
          val (res, _) = dispatcher.update(op).run(iGet(model))
          iSet(res)(model) -> op
        }
      }
      _updates = updater :: _updates

      dispatcher
    }
  }
}

object Dispatcher {

  trait DispatcherFactory[S,A] {
    def apply(iUpdate : (A => State[S, A]) ): Dispatcher[S, A]
    def subscribe(subscriber: (S, A) => S): Dispatcher[S, A]
  }

  //type DispatcherFactory[S,A] = (A => State[S, A]) => Dispatcher[S, A]

  def apply[S,A]() = new Dispatcher[S,A] {
    override def update: (A) => State[S, A] = { op =>
      _updates match {
        case h::Nil => h(op)
        case h::t =>
          t.foldLeft(h(op)) { (accu, update) =>
            val r2 = for {
              _ <- accu
              _ <- update(op)
            } yield (op)
            r2
          }
        case Nil => State[S, A] {model => model -> op}
      }
    }
  }
}
