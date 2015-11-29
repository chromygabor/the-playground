package com.chromy.frpui.fw.javafx

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.fw.core._

/**
 * Created by cry on 2015.11.01..
 */
trait Renderer[M <: BaseModel, C] extends ((M, C) => SideEffect) {

  def ++(renderer: Renderer[M, C]): Renderer[M, C] = {
    val parentThis = this
    new Renderer[M, C] {
      private val parent = parentThis

      override def apply(model: M, context: C): SideEffect = {
        SideEffect {
          parent(model, context).run()
          renderer(model, context).run()
        }
      }
    }
  }
}

object Renderer {

  def apply[M <: BaseModel](): Renderer[M, Null] = new Renderer[M, Null] {
    override def apply(model: M, context: Null): SideEffect = SideEffect()
  }

  def apply[M <: BaseModel](f: M => SideEffect): Renderer[M, Null] = new Renderer[M, Null] {
    //      override val subscriber: (M) => SideEffect = { model => f(model) }
    override def apply(model: M, context: Null): SideEffect = f(model)
    override def toString(): String = "Renderer"
  }

  def apply[M <: BaseModel](initialState: M)(f: (M, M) => SideEffect): Renderer[M, Null] = new Renderer[M, Null] {
    val prevValue = new AtomicReference[M](initialState)

    override def apply(in: M, context: Null): SideEffect = {
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

object RendererChain {
  type RendererChain[T] = SideEffectChain[T, RenderContext]
  def apply[T](): SideEffectChain[T, RenderContext] = new SideEffectChain[T, RenderContext] { }
}
