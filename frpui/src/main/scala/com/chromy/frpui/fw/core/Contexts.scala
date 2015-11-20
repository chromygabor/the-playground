package com.chromy.frpui.fw.core

import com.chromy.frpui.Renderer

/**
 * Created by chrogab on 2015.11.18..
 */

//************ FrpApp contexts

trait UpdateContext {
  def getService[B : Manifest]: B
  def onAction(action: Event): Unit
}

trait RenderContext {
  def updateContext: UpdateContext
  def subscribeToService[B <: BaseService: Manifest](renderer: Renderer[B, RenderContext]): Unit
}


//******** Other contexts
trait BehaviorContext {
  def getService[B : Manifest]: B
  def onAction(action: Event): Unit
}

object BehaviorContext {
  def apply(context: UpdateContext, uid: Uid): BehaviorContext = new BehaviorContext {
    override def getService[B: Manifest]: B = context.getService[B]

    override def onAction(action: Event): Unit = ???
  }
}

trait ControllerContext[M <: BaseModel] extends RenderContext {
  def call(f: (UpdateContext, Uid) => Unit): Unit
}

object ControllerContext {
  def apply[M <: BaseModel](context: RenderContext, model: M) : ControllerContext[M] = new ControllerContext[M] {
    override def call(f: (UpdateContext, Uid) => Unit): Unit = f.apply(updateContext, model.uid)

    override def updateContext: UpdateContext = context.updateContext

    override def subscribeToService[B <: BaseService : Manifest](renderer: Renderer[B, RenderContext]): Unit = ???
  } 
}
