package com.chromy.frpui.core

/**
 * Created by cry on 2015.10.18..
 */
trait BaseComponent {
  type M <: BaseModel
}

trait Component[A <: BaseModel] extends BaseComponent {
  type M = A
}
