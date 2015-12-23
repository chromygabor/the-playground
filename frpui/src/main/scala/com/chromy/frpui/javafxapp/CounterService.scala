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

  def firstAvailable(actual: Option[(Uid, CounterState)]): Option[(Uid, CounterState)]

  def nextAvailable(active: Option[CounterState]): Option[(Uid, CounterState)]

  def prevAvailable(active: Option[CounterState]): Option[(Uid, CounterState)]


  def counter(uid: Uid): Option[(Uid, CounterState)]
}

trait CounterStateChanged extends Event

case class CounterAdded(counterUid: Uid, value: Int) extends CounterStateChanged

case class CounterChanged(counterUid: Uid, value: Int) extends CounterStateChanged

case class CounterRemoved(counterUid: Uid) extends CounterStateChanged

case class CounterState(uid: Uid, value: Int, hasPrev: Boolean, hasNext: Boolean, index: Int)


/**
 * Add a new counter
 * @param counterUid: Uid of the counter to add
 */
case class AddCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  override def apply(model: CounterServiceImpl, context: UpdateContext): Result[CounterServiceImpl] = {
    val newValue = 0
    val newModel = model.copy(counters = counterUid -> newValue :: model.counters)

    Result(newModel) { (_, _) =>
      CounterAdded(counterUid, newValue)
    }
  }
}

/**
 * Increment a counter
 * @param counterUid: Uid of the counter to add
 */
case class IncrementCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  override def apply(model: CounterServiceImpl, context: UpdateContext): Result[CounterServiceImpl] = {
    val (newCounters, newValue) = CounterServiceImpl.updateCounter(model.counters, counterUid, _ + 1)
    val newModel = model.copy(counters = newCounters)

    Result(newModel) { (_, _) =>
      CounterChanged(counterUid, newValue)
    }
  }
}

/**
 * Decrement a counter
 * @param counterUid: Uid of the counter to add
 */
case class DecrementCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  override def apply(model: CounterServiceImpl, context: UpdateContext): Result[CounterServiceImpl] = {
    val (newCounters, newValue) = CounterServiceImpl.updateCounter(model.counters, counterUid, _ - 1)
    val newModel = model.copy(counters = newCounters)

    Result(newModel) { (_, _) =>
      CounterChanged(counterUid, newValue)
    }
  }
}

/**
 * Decrement a counter
 * @param counterUid: Uid of the counter to add
 */
case class CloseCounter(counterUid: Uid) extends Command[CounterServiceImpl] {
  override def apply(model: CounterServiceImpl, context: UpdateContext): Result[CounterServiceImpl] = {
    val newModel = model.copy(counters = model.counters.filterNot { case (uid, _) => uid == counterUid })
    Result(newModel) { (_, _) =>
      CounterRemoved(counterUid)
    }
  }
}

object CounterServiceImpl extends Behavior[CounterServiceImpl] {
  def updateCounter(original: List[(Uid, Int)], counterUid: Uid, f: Int => Int): (List[(Uid, Int)], Int) = {
    original.foldRight((List.empty[(Uid, Int)], 0)) { case ((uid, value), (list, newValue)) =>
      if (uid == counterUid) {
        val newValue = f(value)
        (uid -> newValue :: list, newValue)
      }
      else {
        (uid -> value :: list, newValue)
      }
    }
  }
}

case class CounterServiceImpl(uid: Uid = Uid(), counters: List[(Uid, Int)] = Nil) extends Service[CounterService, CounterServiceImpl] with LazyLogging {

  override def handle(implicit context: UpdateContext): EventHandler[CounterServiceImpl] = EventHandler {
    case Init =>
      this
  }

  override def api(context: UpdateContext): CounterService = new CounterService {
    private val model: CounterServiceImpl = CounterServiceImpl.this

    override def decrement(sUid: Uid) = Action(model) { (model, _) =>
      Result(model) { (_, _) =>
        DecrementCounter(sUid)
      }
    }

    override def increment(sUid: Uid) = Action(model) { (model, _) =>
      Result(model) { (_, _) =>
        IncrementCounter(sUid)
      }
    }

    override def close(sUid: Uid) = Action(model) { (model, _) =>
      Result(model) { (_, _) =>
        CloseCounter(sUid)
      }

    }

    override def addCounter = Action(model) { (model, context) =>
      Result(model) { (_, _) =>
        logger.debug("Sleeping")
        Thread.sleep(1000)
        AddCounter(Uid())
      }
    }

    private val tIndexedCounters = counters.zipWithIndex
    private lazy val size = tIndexedCounters.size
    private lazy val indexedCounters = tIndexedCounters.map { case ((uid, value), index) => uid -> CounterState(uid, value, index != 0, index < size - 1, index) }.toMap

    override def firstAvailable(actual: Option[(Uid, CounterState)]): Option[(Uid, CounterState)] = actual match {
      case None =>
        indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == 0 }
      case Some((_, counterState)) =>
        indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == index - 1 } match {
          case s@Some(_) => s
          case None => indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == 0 }
        }
    }
    override def counter(uid: Uid): Option[(Uid, CounterState)] = indexedCounters.get(uid).map { counterState => uid -> counterState }

    override def nextAvailable(active: Option[CounterState]): Option[(Uid, CounterState)] = active match {
      case None =>
        indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == 0 }
      case Some(counterState) =>
        indexedCounters.find { case (_, c@CounterState(_, value, _, _, index)) => index == counterState.index + 1 } match {
          case s@Some(_) => s
          case None => indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == size - 1 }
        }
    }

    override def prevAvailable(active: Option[CounterState]): Option[(Uid, CounterState)] = active match {
      case None =>
        indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == 0 }
      case Some(counterState) =>
        indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == counterState.index - 1 } match {
          case s@Some(_) => s
          case None => indexedCounters.find { case (_, CounterState(_, value, _, _, index)) => index == 0 }
        }
    }
  }


} 
