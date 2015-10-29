package com.chromy.frpui.core

import monocle.macros.GenLens
import rx.lang.scala.schedulers.{ComputationScheduler, TrampolineScheduler, ImmediateScheduler}
import rx.lang.scala.{Observer, Subject}

/**
 * Created by cry on 2015.10.18..
 */
trait BaseComponent {
  type M <: BaseModel
}

trait Component[A <: BaseModel] extends BaseComponent {
  type M = A
}


