package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx._
import monocle.Lens
import rx.lang.scala.{Observable, Observer}

/**
 * Created by cry on 2015.07.05..
 */

trait Router[T] {
  val changes: Observable[T]
  val channel: Observer[Action]
  val chain: UpdateChain[T]

  def map[B](lens: Lens[T, B]): Router[B] = {
    val parent = this
    new Router[B] {
      val changes = parent.changes.map { in => lens.get(in) }
      val channel = parent.channel
      val chain = parent.chain.fromLens(lens)
    }
  }

}

