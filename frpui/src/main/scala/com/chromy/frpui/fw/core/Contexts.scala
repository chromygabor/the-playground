package com.chromy.frpui.fw.core

import com.chromy.frpui.fw.javafx.Renderer

/**
 * Created by chrogab on 2015.11.18..
 */

trait UpdateContext {
  def getService[B : Manifest]: B
  def !(action: Event): Unit
}

trait RenderContext {
  def updateContext: UpdateContext
  def subscribeToService[B <: BaseService: Manifest](renderer: Renderer[B, RenderContext]): Unit
}

trait ControllerContext[M <: BaseModel] extends RenderContext {
  def call(f: M => Action[M]): Unit 
}

object ControllerContext {
  def apply[M <: BaseModel](context: RenderContext, model: M) : ControllerContext[M] = new ControllerContext[M] {
    override def call(f: M => Action[M]): Unit = context.updateContext.!(f.apply(model))

    override def updateContext: UpdateContext = context.updateContext

    override def subscribeToService[B <: BaseService : Manifest](renderer: Renderer[B, RenderContext]): Unit = ???
  } 
}
