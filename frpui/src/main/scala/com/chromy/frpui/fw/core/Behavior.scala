package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */

trait BehaviorAction[A] {
  def apply(context: BehaviorContext[A], model: A): A
}

trait BehaviorContext[A] extends Context {
  def fire(behaviorAction: BehaviorAction[A]): Unit
}

object BehaviorContext {
  def apply[A <: BaseModel](context: Context, model: A): BehaviorContext[A] = new BehaviorContext[A] {
    override def fire(behaviorAction: BehaviorAction[A]): Unit = {
      def newF(context: Context, model: A): A = behaviorAction(BehaviorContext[A](context, model), model)
      val delayedEvent: DelayedEvent[A] = DelayedEvent(model.uid, newF)
      context.onAction(delayedEvent)
    }

    override def getService[B: Manifest]: B = context.getService[B]

    override def onAction(action: Event): Unit = context.onAction(action)
  }
}

trait Behavior[M <: BaseModel] {
  object Action {
    def apply(inF: (BehaviorContext[M], M) => M): BehaviorAction[M] = new BehaviorAction[M] {
      override def apply(context: BehaviorContext[M], model: M): M = inF(context, model)
    }
  }
}
