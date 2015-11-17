package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */

trait BehaviorAction[A <: BaseModel]  extends Event {
  def apply(uid: Uid): UpdateAction[A]// = new UpdateAction(uid, { (context, model) => apply(BehaviorContext(context, uid), model) })
}

trait BehaviorContext extends Context

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

trait Behavior[M <: BaseModel] {
  import com.chromy.frpui.fw.core.{PostOrderAction => PostA, PreOrderAction => PreA, UpdateAction}

  object Handler {
    def apply(iHandler: PartialFunction[Event, BehaviorAction[M]]): PartialFunction[Event, BehaviorAction[M]] = iHandler
  }
  
  
  object Action {
    def apply(action: (BehaviorContext, M) => M): BehaviorAction[M] = new BehaviorAction[M] {
      override def apply(iUid: Uid): UpdateAction[M] = new UpdateAction[M] with PreA {
        override def uid: Uid = iUid

        override def apply(context: Context, model: M): M = action(BehaviorContext(context, iUid), model)
      }
    }
  }

  object PostOrderAction {
    def apply(action: (BehaviorContext, M) => M): BehaviorAction[M] = new BehaviorAction[M] {
      override def apply(iUid: Uid): UpdateAction[M] = new UpdateAction[M] with PostA {
        override def uid: Uid = iUid
        override def apply(context: Context, model: M): M = action(BehaviorContext(context, iUid), model)
      }
    }
  }

  def eventHandler(model: M)(implicit context: Context): EventHandler[M] = new EventHandler[M] {
    override val handle: PartialFunction[Event, M] = {
      case e if handler(model).isDefinedAt(e) =>
        val updateAction = handler(model)(e).apply(model.uid)
        updateAction.apply(BehaviorContext(context, model.uid), model)
    }
  }
  
  def handler(model: M): PartialFunction[Event, BehaviorAction[M]] = new PartialFunction[Event, BehaviorAction[M]] {
    override def isDefinedAt(x: Event): Boolean = false
    override def apply(v1: Event): BehaviorAction[M] = ???
  }

  def command(f: (BehaviorContext, M) => Unit): ((Context, M) => Unit) = new ((Context, M) => Unit) {
    override def apply(context: Context, model: M): Unit = {
      f(BehaviorContext(context, model.uid), model) 
    }
  }
}
