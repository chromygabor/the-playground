package com.chromy.frpui.fw.core

import com.chromy.frpui.Renderer

/**
 * Created by chrogab on 2015.11.18..
 */

trait Context {
  def getService[B : Manifest]: B

  def onAction(action: Event): Unit
}

trait ModelContext extends Context {
  
}

object ModelContext {
  //def apply()
}

trait BehaviorContext {
  def getService[B : Manifest]: B
  def onAction(action: Event): Unit
}

object BehaviorContext {
  def apply(context: Context, uid: Uid): BehaviorContext = new BehaviorContext {
    override def getService[B: Manifest]: B = context.getService[B]

    override def onAction(action: Event): Unit =
      action match {
        case e: BehaviorAction[_] => context.onAction(e(uid))
        case e => context.onAction(e)
      }
  }
}

trait RendererContext {
  def context: Context
  def subscribeToService[B <: BaseService: Manifest](renderer: Renderer[B, RendererContext]): Unit
}

trait ControllerContext[M <: BaseModel] extends RendererContext {
  def command(f: (BehaviorContext, M) => Unit): Unit
}

object ControllerContext {
  def apply[M <: BaseModel](context: RendererContext, model: M) : ControllerContext[M] = new ControllerContext[M] {
    override def command(f: (BehaviorContext, M) => Unit): Unit = ??? //f.apply(context, model)

    override def subscribeToService[B <: BaseService: Manifest](renderer: Renderer[B, RendererContext]): Unit = context.subscribeToService(renderer)
  }
}
