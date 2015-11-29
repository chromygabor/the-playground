package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.28..
 */
trait Action[M <: BaseModel] extends Event {
  def isDefinedAt(model: BaseModel): Boolean
  def apply(model: M, context: UpdateContext): Result[M]
}

object Action {
  def apply[M <: BaseModel](model: M)(f: (M, UpdateContext) => Result[M]): Action[M] = new Action[M] {
    override def isDefinedAt(other: BaseModel): Boolean = other.uid == model.uid
    override def apply(model: M, context: UpdateContext): Result[M] = f(model, context)
  }
}

trait PersistableEvent

abstract class Command[M <: BaseModel: Manifest] extends Action[M] with PersistableEvent {
  private val manifestOfParam = manifest[M] 
  override def isDefinedAt(other: BaseModel): Boolean = manifestOfParam.runtimeClass.isAssignableFrom(other.getClass)
  override def apply(model: M, context: UpdateContext): Result[M]
}

trait Result[M] {
  val model: M
  val event: (M, UpdateContext) => Event
}

object Result {

  def apply[M](iModel: M)(f: (M, UpdateContext) => Event ) : Result[M] = new Result[M] {
    override val model: M = iModel
    override val event: (M, UpdateContext) => Event = f
  }

  def unapply[M](result: Result[M]): Option[(M, (M, UpdateContext) => Event)] = {
    Some((result.model, result.event))
  }
}
