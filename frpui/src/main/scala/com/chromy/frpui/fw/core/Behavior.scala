package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */


trait Behavior[M <: BaseModel] {
  def command(f: (BehaviorContext, M) => Command[_]): ((UpdateContext, Uid) => Unit) = new ((UpdateContext, Uid) => Unit) {
    override def apply(context: UpdateContext, iUid: Uid): Unit = {
      val command = new Action[M] {
        override def apply(context: UpdateContext, model: M): Unit = {
          val action = f(BehaviorContext(context, model.uid), model)
          context.onAction(action)
        }
        override def uid: Uid = iUid
      }
      context.onAction(command)
    }
  }
  
}
