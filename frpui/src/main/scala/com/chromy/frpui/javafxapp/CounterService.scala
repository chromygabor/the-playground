package com.chromy.frpui.javafxapp

import com.chromy.frpui.fw.core._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.ListMap

/**
 * Created by cry on 2015.11.06..
 */
trait CounterService {

  def addCounter: Action[_ <: Service[CounterService, _]]

  def increment(uid: Uid): Action[_ <: Service[CounterService, _]]

  def decrement(uid: Uid): Action[_ <: Service[CounterService, _]]

  def close(sUid: Uid): Action[_ <: Service[CounterService, _]]

}

case class CounterAdded(counterUid: Uid, value: Int) extends Event

case class CounterChanged(counterUid: Uid, value: Int) extends Event

/**
 * Add a new counter
 * @param counterUid: Uid of the counter to add
 */
case class AddCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  override def apply(model: CounterServiceImpl, context: UpdateContext): Result[CounterServiceImpl] = {
    val newModel = model.copy(counters = model.counters.updated(counterUid, 0))
    Result(newModel) { (_,_) =>
      CounterAdded(counterUid, newModel.counters(counterUid))
    }
  }
}

/**
 * Increment a counter
 * @param counterUid: Uid of the counter to add
 */
case class IncrementCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  override def apply(model: CounterServiceImpl, context: UpdateContext): Result[CounterServiceImpl] = {
    val newModel = model.copy(counters = model.counters.updated(counterUid, model.counters.getOrElse(counterUid, 0) + 1))
    Result(newModel) { (_,_) =>
      CounterChanged(counterUid, newModel.counters(counterUid))
    }
  }
}

/**
 * Decrement a counter
 * @param counterUid: Uid of the counter to add
 */
case class DecrementCounter(counterUid: Uid) extends Command[CounterServiceImpl] {

  override def apply(model: CounterServiceImpl, context: UpdateContext): Result[CounterServiceImpl] = {
    val newModel = model.copy(counters = model.counters.updated(counterUid, model.counters.getOrElse(counterUid, 0) - 1))
    Result(newModel) { (_,_) =>
      CounterChanged(counterUid, newModel.counters(counterUid))
    }
  }
  
}

object CounterServiceImpl extends Behavior[CounterServiceImpl] {

}

case class CounterServiceImpl(uid: Uid = Uid(), counters: ListMap[Uid, Int] = ListMap()) extends Service[CounterService, CounterServiceImpl] with LazyLogging{
  override def handle(implicit context: UpdateContext): EventHandler[CounterServiceImpl] = EventHandler {
    case Init =>
      this
  }

  override def api(context: UpdateContext): CounterService = new CounterService {

    override def decrement(sUid: Uid) = Action(CounterServiceImpl.this) { (model, _) =>
      Result(model) { (_, _) =>
        DecrementCounter(sUid)
      }
    }

    override def increment(sUid: Uid) = Action(CounterServiceImpl.this) { (model, _) =>
      Result(model) { (_, _) =>
        IncrementCounter(sUid)
      }
    }

    override def close(sUid: Uid) = ???

    override def addCounter = Action(CounterServiceImpl.this) { (model, context) =>
      Result(model) { (_, _) =>
        logger.debug("Sleeping")
        Thread.sleep(1000)
        AddCounter(Uid())
      }
    }
  }
} 
