package com.chromy.frpui.javafxapp

import com.chromy.frpui.fw.core._

import scala.collection.immutable.ListMap

/**
 * Created by cry on 2015.11.06..
 */
trait CounterService {
  def addCounter(): Command[_ <: Service[CounterService, _]]

  def increment(uid: Uid): Command[_ <: Service[CounterService, _]]
  
  def decrement(uid: Uid): Command[_ <: Service[CounterService, _]]

  def close(sUid: Uid): Command[_ <: Service[CounterService, _]]

}


object CounterServiceImpl extends Behavior[CounterServiceImpl] {
  
}

case class CounterAdded(counterUid: Uid, value: Int) extends Event
case class CounterChanged(counterUid: Uid, value: Int) extends Event

/**
 * Add a new counter
 * @param counterUid: Uid of the counter to add
 */
case class AddCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  def apply(context: UpdateContext, model: CounterServiceImpl): CounterServiceImpl = 
    model.copy(counters = model.counters.updated(counterUid, 0))
  
  def notification(context: UpdateContext, newModel: CounterServiceImpl): Event = 
    CounterAdded(counterUid, newModel.counters(counterUid))
}

/**
 * Increment a counter
 * @param counterUid: Uid of the counter to add
 */
case class IncrementCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  def apply(context: UpdateContext, model: CounterServiceImpl): CounterServiceImpl =
    model.copy(counters = model.counters.updated(counterUid, model.counters.getOrElse(counterUid, 0) + 1))

  def notification(context: UpdateContext, newModel: CounterServiceImpl): Event =
    CounterChanged(counterUid, newModel.counters(counterUid))
}

/**
 * Decrement a counter
 * @param counterUid: Uid of the counter to add
 */
case class DecrementCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  def apply(context: UpdateContext, model: CounterServiceImpl): CounterServiceImpl =
    model.copy(counters = model.counters.updated(counterUid, model.counters.getOrElse(counterUid, 0) - 1))


  def notification(context: UpdateContext, newModel: CounterServiceImpl): Event =
    CounterChanged(counterUid, newModel.counters(counterUid))
}

case class CounterServiceImpl(uid: Uid = Uid(), counters: ListMap[Uid, Int] = ListMap()) extends Service[CounterService, CounterServiceImpl] {
  override def handle(implicit context: UpdateContext): EventHandler[CounterServiceImpl] = EventHandler {
    case Init =>
      println("CounterService Init was called")
      this
  }

  override def api(context: UpdateContext): CounterService = new CounterService {

    override def decrement(sUid: Uid) = DecrementCounter(sUid)
    override def increment(sUid: Uid) = IncrementCounter(sUid)
    override def close(sUid: Uid) = ???
    
    override def addCounter() =  {
      println("Sleeping")
      Thread.sleep(1000)
      AddCounter(Uid())
    }
  }
} 
