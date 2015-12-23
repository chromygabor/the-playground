package com.chromy.frpui.fw.javafx

import java.util.concurrent.atomic.AtomicReference
import RendererChain.RendererChain
import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.Renderer
import monocle.Lens

/**
 * Created by cry on 2015.11.01..
 */


case class ChildController[M <: BaseModel, B <: BaseController](controller: B, getter: M => B#C) {
  def init(context: RenderContext, render: RendererChain[M], initialState: M): SideEffect = {
    val childRender = render.map(getter).asInstanceOf[RendererChain[controller.C]]
    val childState = getter(initialState).asInstanceOf[controller.C]
    controller.init(context, childRender, childState)
  }
}

trait BaseController {
  type C <: BaseModel

  protected lazy val children: List[ChildController[C, _ <: BaseController]] = List.empty

  protected def child[B <: BaseController](controller: B)(getter: C => B#C) = ChildController[C, B](controller, getter)
  
  private[this] val _initialState = new AtomicReference[(C, RendererChain[C])]()

  protected def initialState: C = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._1
  }

  protected def render: RendererChain[C] = {
    val r = _initialState.get
    if (r == null) throw new IllegalAccessError("The initialState is accessible only after the init method was called")
    r._2
  }
  
  protected val renderer: Renderer[C, RenderContext]

  def init(context: RenderContext, render: RendererChain[C], initialState: C): SideEffect = {
    val distinctRender = render.distinctUntilChanged  
    _initialState.set((initialState, distinctRender))
    distinctRender.subscribe(renderer)

    val se = render.update(render.lastItem.getOrElse(initialState), context)
    
    children.foldLeft(se) { case (se, child) =>
      se ++ child.init(context, distinctRender, initialState)
    }
  }

  def Renderer(f: (ControllerContext[C], C) => SideEffect): Renderer[C, RenderContext] = new Renderer[C, RenderContext] {
    override def apply(model: C, context: RenderContext): SideEffect = {
      f(ControllerContext[C](context, model), model)
    }
    override def toString(): String = "Renderer"
  }

  def Renderer(f: (ControllerContext[C], C, C) => SideEffect): Renderer[C, RenderContext] = new Renderer[C, RenderContext] {
    val prevValue = new AtomicReference[C](initialState)

    override def apply(in: C, context: RenderContext): SideEffect = {

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
}

/**
 * Do not do any javafx related stuffs in the constructor, you can't add anything at initialization time, because it shouldn't
 * matter which time you run that sg.
 * @tparam COMP
 */
trait Controller[COMP <: BaseModel] extends BaseController {
  type C = COMP
}
