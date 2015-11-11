package com.chromy.frpui

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.fw.core.{Context, SideEffect}

/**
 * Created by cry on 2015.11.01..
 */
trait Renderer[A] extends ((A, Context) => SideEffect) {

  def ++(renderer: Renderer[A]): Renderer[A] = {
    val parentThis = this
    new Renderer[A] {
      private val parent = parentThis

      override def apply(model: A, context: Context): SideEffect = {
        SideEffect {
          parent(model, context).run()
          renderer(model, context).run()
        }
      }
    }
  }
}

object Renderer {

  def apply[C](): Renderer[C] = new Renderer[C] {
    //override val subscriber: (C) => SideEffect = { _ => SideEffect() }
    override def apply(model: C, context: Context): SideEffect = SideEffect()
  }

  def apply[C](f: C => SideEffect): Renderer[C] = new Renderer[C] {
    //      override val subscriber: (C) => SideEffect = { model => f(model) }
    override def apply(model: C, context: Context): SideEffect = f(model)
    override def toString(): String = "Renderer"
  }

  def apply[C](initialState: C)(f: (C, C) => SideEffect): Renderer[C] = new Renderer[C] {
    val prevValue = new AtomicReference[C](initialState)

    override def apply(in: C, context: Context): SideEffect = {
      if (in != prevValue.get) {
        val oldValue = prevValue.get
        prevValue.set(in)
        f(oldValue, in)
      } else {
        SideEffect()
      }
    }
    override def toString(): String = "Renderer"
  }
  
}