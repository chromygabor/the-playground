package com.chromy.reactiveui.core

import java.util.concurrent.Executor

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
  def changes: Observable[A]
  def channel: Observer[Action]
  def chain: UpdateChain[A]

  def chainExecutor: Executor
  def changesExecutor: Executor

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
        override val changes = parent.changes.map { in => lens.get(in) }
        override val channel = parent.channel
        override val chain = parent.chain.map(lens)
        override val chainExecutor: Executor = parent.chainExecutor
        override val changesExecutor: Executor = parent.changesExecutor
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

