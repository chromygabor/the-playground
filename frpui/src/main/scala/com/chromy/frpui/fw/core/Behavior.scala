package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */

class BehaviorAction[A <: BaseModel](apply: (BehaviorContext, A) => A)  extends Event {
  def asEvent(uid: Uid): Action[A] = Action(uid, { (context, model) => apply(BehaviorContext(context, uid), model) })
}

trait BehaviorContext extends Context 

object BehaviorContext {
  def apply(context: Context, uid: Uid): BehaviorContext = new BehaviorContext {
    override def getService[B: Manifest]: B = context.getService[B]

    override def onAction(action: Event): Unit = 
      action match {
        case e: BehaviorAction[_] => context.onAction(e.asEvent(uid))
        case e => context.onAction(e)

      }
  }
}

trait Behavior[M <: BaseModel] {
  object Action {
    def apply(action: (BehaviorContext, M) => M): BehaviorAction[M] = new BehaviorAction[M](action) 
  }
}
