package com.chromy.frpui.fw.javafx

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.Renderer
import com.chromy.frpui.fw.core._

/**
 * Created by cry on 2015.11.01..
 */
trait BaseController {
  type C <: BaseModel

  private[this] var _initialState = new AtomicReference[(C, Event => Unit, SideEffectChain[C])]()

  protected def initialState: C = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._1
  }

  protected def fire(action: BehaviorAction[C])(implicit model: C) = {
    def newF(context: Context, model: C): C = action(BehaviorContext[C](context, model), model)
    val delayedEvent = DelayedEvent[C](model.uid, newF)
    channel(delayedEvent)
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

  protected def renderer: Renderer[C]

  def init(channel: Event => Unit, render: SideEffectChain[C], initialState: C): SideEffect = {
    _initialState.set((initialState, channel, render))
    render.distinctUntilChanged.subscribe(renderer.subscriber)
    render.update(initialState)
  }

  protected object Renderer {
    def apply(): Renderer[C] = new Renderer[C] {
      override val subscriber: (C) => SideEffect = { _ => SideEffect() }
    }

    def apply(f: C => SideEffect): Renderer[C] = new Renderer[C] {
      override val subscriber: (C) => SideEffect = { model => f(model) }
    }

    def apply(f: (C, C) => SideEffect): Renderer[C] = new Renderer[C] {
      override val subscriber: (C) => SideEffect = {
        val prevValue = new AtomicReference[C](initialState)
        val res: C => SideEffect = { in =>
          val res = if(in != prevValue.get) {
            val oldValue = prevValue.get
            prevValue.set(in)
            f(oldValue, in)
          } else {
            SideEffect()
          }
          res
        }
        res
      }
    }
  }

}

trait Controller[COMP <: BaseModel] extends BaseController {
  type C = COMP
}
