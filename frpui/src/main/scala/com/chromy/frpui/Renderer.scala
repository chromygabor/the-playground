package com.chromy.frpui

import com.chromy.frpui.fw.core.SideEffect

/**
 * Created by cry on 2015.11.01..
 */
trait Renderer[A] {
  def subscriber: A => SideEffect

  def ++(renderer: Renderer[A]): Renderer[A] = {
    val parent = this
    new Renderer[A] {
      override def subscriber: (A) => SideEffect = { model =>
        SideEffect {
          parent.subscriber(model).run()
          renderer.subscriber(model).run()
        }
      }
    }
  }
}
