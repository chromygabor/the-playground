package com.chromy.frpui

import com.chromy.frpui.fw.core.SideEffect

/**
 * Created by cry on 2015.11.01..
 */
trait Renderer[A] extends ((A) => SideEffect) {

  def ++(renderer: Renderer[A]): Renderer[A] = {
    val parentThis = this
    new Renderer[A] {
      private val parent = parentThis

      override def apply(model: A): SideEffect = {
        SideEffect {
          parent(model).run()
          renderer(model).run()
        }
      }
    }
  }
}
