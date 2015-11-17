package com.chromy.frpui.fw.javafx

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.Renderer
import com.chromy.frpui.fw.core._

/**
 * Created by cry on 2015.11.01..
 */


trait ControllerContext[M] {
  def command(f: (Context, M) => Unit): Unit
}

object ControllerContext {
  def apply[M](context: Context, model: M) : ControllerContext[M] = new ControllerContext[M] {
    override def command(f: (Context, M) => Unit): Unit = f.apply(context, model)
  }
}

trait BaseController {
  type C <: BaseModel

  private[this] val _initialState = new AtomicReference[(C, SideEffectChain[C])]()
  private[this] val _context = new AtomicReference[Context]()

  protected def initialState: C = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._1
  }

  protected def onAction(action: Event)(implicit model: C) = {
    action match {
      case e: BehaviorAction[_] => context.onAction(e(model.uid))
      case e => context.onAction(e)
    }
  }

  protected def context: Context = _context.get

  protected def render: SideEffectChain[C] = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._2
  }

  protected val renderer: Renderer[C]

  def init(context: Context, render: SideEffectChain[C], initialState: C): SideEffect = {
    val distinctRender = render.distinctUntilChanged  
    _initialState.set((initialState, distinctRender))
    distinctRender.subscribe(renderer)

    render.update(render.lastItem.getOrElse(initialState), context)
  }

  def asRenderer(f: (ControllerContext[C], C) => SideEffect): Renderer[C] = new Renderer[C] {
    override def apply(model: C, context: Context): SideEffect = {
      f(ControllerContext[C](context, model), model)
    }
    override def toString(): String = "Renderer"
  }

  def asRenderer(f: (ControllerContext[C], C, C) => SideEffect): Renderer[C] = new Renderer[C] {
    val prevValue = new AtomicReference[C](initialState)

    override def apply(in: C, context: Context): SideEffect = {
      _context.set(context)

      if (in != prevValue.get) {
        val oldValue = prevValue.get
        prevValue.set(in)
        f(ControllerContext[C](context, in), oldValue, in)
      } else {
        SideEffect()
      }
    }
    override def toString(): String = "Renderer"
  }
  
  
  protected object Renderer {
    def apply(): Renderer[C] = new Renderer[C] {
      override def apply(model: C, context: Context): SideEffect = {
        _context.set(context)
        SideEffect()
      }
    }

    def apply(f: C => SideEffect): Renderer[C] = new Renderer[C] {
      override def apply(model: C, context: Context): SideEffect = {
        _context.set(context)
        f(model)
      }
      override def toString(): String = "Renderer"
    }

    def apply(f: (C, C) => SideEffect): Renderer[C] = new Renderer[C] {
      val prevValue = new AtomicReference[C](initialState)

      override def apply(in: C, context: Context): SideEffect = {
        _context.set(context)

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
