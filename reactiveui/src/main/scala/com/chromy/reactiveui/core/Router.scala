package com.chromy.reactiveui.core

import monocle.Lens
import rx.lang.scala.{Observable, Observer}

/**
 * Created by cry on 2015.07.05..
 */

trait RouterMapper[B] {
  def apply(f: (Action, B) => B): Router[B]
  def apply(f: (Action, B, B) => B): Router[B]
}

trait Router[A] {
  def changes: Observable[A]
  def channel: Observer[Action]
  def chain: UpdateChain[A]

  def map[B](lens: Lens[A, B]): RouterMapper[B] = {
    val parent = this
    new RouterMapper[B] {
      def apply(f: (Action, B) => B): Router[B] = {
        def realSubscriber: (Action, B, B) => B = { (action, originalModel, model) =>
          f(action, model)
        }
        apply(realSubscriber)
      }
      def apply(f: (Action, B, B) => B): Router[B] = new Router[B] {
        val changes = parent.changes.map { in => lens.get(in) }
        val channel = parent.channel
        val chain = parent.chain.fromLens(lens)
        chain.subscribe(f)
      }
    }
  }

  def mapper: RouterMapper[A] = {
    val parent = this
    new RouterMapper[A] {
      def apply(f: (Action, A) => A): Router[A] = {
        parent.chain.subscribe(f)
        parent
      }
      def apply(f: (Action, A, A) => A): Router[A] = {
        parent.chain.subscribe(f)
        parent
      }
    }
  }
}

