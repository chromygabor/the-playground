package com.chromy.reactiveui

import com.chromy.reactiveui.Router.RouterModel
import com.chromy.reactiveui.myjavafx.{Action, Module}
import monocle.Lens
import rx.lang.scala.{Observer, Observable}

/**
 * Created by cry on 2015.07.05..
 */
trait Router[T <: RouterModel] {
  val changes: Observable[T#Model]
  val channel: Observer[Action]
  val chain: UpdateChain[T#Model]

  def map[B <: Module](lens: Lens[T#Model, B#Model]): Router[B] = ???
}

object Router {

  type RouterModel = {type Model}

}
