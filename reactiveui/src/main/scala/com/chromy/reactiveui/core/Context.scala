package com.chromy.reactiveui.core

import java.util.concurrent.Executor

import com.chromy.reactiveui.core.misc.SideChain
import monocle.Lens
import rx.lang.scala.schedulers.ImmediateScheduler
import rx.lang.scala.{Scheduler, Observable, Observer}
import rx.schedulers.Schedulers

import scala.concurrent.ExecutionContext

/**
 * Created by cry on 2015.07.05..
 */

trait ContextMapper[B] {
  def apply(f: (Action, B, B) => B): Context[B]
}

trait Context[A] {
  //def render: SideChain[A]
  //def changes: Observable[A]
  def changes: SideChain[A]
  def channel: Observer[Action]
  def chain: UpdateChain[A]
  def initialState: A

  def backgroundExecutor: ExecutionContext

  def map[B](lens: Lens[A, B]): ContextMapper[B] = {
    val parent = this
    new ContextMapper[B] {
      def apply(f: (Action, B) => B): Context[B] = {
        def realSubscriber: (Action, B, B) => B = { (action, originalModel, model) =>
          f(action, model)
        }
        apply(realSubscriber)
      }
      def apply(f: (Action, B, B) => B): Context[B] = new Context[B] {
        //override val render = parent.render.to(lens)
        override val changes = parent.changes.map { in => lens.get(in) }
        override val channel = parent.channel
        override val chain = parent.chain.map(lens)
        override val initialState = lens.get(parent.initialState)

        override val backgroundExecutor: ExecutionContext = parent.backgroundExecutor
        chain.subscribe(f)
      }
    }
  }

  def mapper: ContextMapper[A] = {
    val parent = this
    new ContextMapper[A] {
      def apply(f: (Action, A, A) => A): Context[A] = {
        parent.chain.subscribe(f)
        parent
      }
    }
  }
}

