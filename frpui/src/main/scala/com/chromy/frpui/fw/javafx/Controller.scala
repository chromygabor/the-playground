package com.chromy.frpui.fw.javafx

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.Renderer
import com.chromy.frpui.fw.core._

/**
 * Created by cry on 2015.11.01..
 */
trait BaseController {
  type C <: BaseModel

  private[this] val _initialState = new AtomicReference[(C, Event => Unit, SideEffectChain[C])]()

  protected def initialState: C = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._1
  }
  protected def onAction(action: Event)(implicit model: C) = {
    action match {
      case e: BehaviorAction[_] => channel(e(model.uid))
      case e => channel(e)
    }
    
  }

  protected def channel: Event => Unit = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._2
  }

  protected def render: SideEffectChain[C] = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._3
  }

  protected val renderer: Renderer[C]

  def init(channel: Event => Unit, render: SideEffectChain[C], initialState: C): SideEffect = {
    val distinctRender = render.distinctUntilChanged  
    _initialState.set((initialState, channel, distinctRender))
    distinctRender.subscribe(renderer)

    render.update(render.lastItem.getOrElse(initialState))
  }

  protected object Renderer {
    def apply(): Renderer[C] = new Renderer[C] {
      //override val subscriber: (C) => SideEffect = { _ => SideEffect() }
      override def apply(model: C): SideEffect = SideEffect()
    }

    def apply(f: C => SideEffect): Renderer[C] = new Renderer[C] {
      //      override val subscriber: (C) => SideEffect = { model => f(model) }
      override def apply(model: C): SideEffect = f(model)
      override def toString(): String = "Renderer"
    }

    def apply(f: (C, C) => SideEffect): Renderer[C] = new Renderer[C] {
      val prevValue = new AtomicReference[C](initialState)

      override def apply(in: C): SideEffect = {
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

}

/**
 * Do not do any javafx related stuffs in the constructor, you can't add anything at initialization time, because it shouldn't
 * matter which time you run that sg.
 * @tparam COMP
 */
trait Controller[COMP <: BaseModel] extends BaseController {
  type C = COMP
}
