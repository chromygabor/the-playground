package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */

class BehaviorAction[A <: BaseModel](apply: (BehaviorContext, A) => A) {
  def asEvent(uid: Uid): Action[A] = Action(uid, { (context, model) => apply(BehaviorContext(context, uid), model) })
}

trait BehaviorContext extends Context {
  def fire[A <: BaseModel](behaviorAction: BehaviorAction[A]): Unit
  def fire(event: Event): Unit
}

object BehaviorContext {
  def apply(context: Context, uid: Uid): BehaviorContext = new BehaviorContext {
    override def fire[A <: BaseModel](behaviorAction: BehaviorAction[A]): Unit = {
      context.onAction(behaviorAction.asEvent(uid))
    }

    override def fire(event: Event): Unit = {
      context.onAction(event)
    }

    override def getService[B: Manifest]: B = context.getService[B]

    override def onAction(action: Event): Unit = context.onAction(action)
  }
}

trait Behavior[M <: BaseModel] {
  object Action {
    def apply(action: (BehaviorContext, M) => M): BehaviorAction[M] = new BehaviorAction[M](action) 
  }
}
