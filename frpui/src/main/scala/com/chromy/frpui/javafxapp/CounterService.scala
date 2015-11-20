package com.chromy.frpui.javafxapp

import com.chromy.frpui.fw.core._

import scala.collection.immutable.ListMap

/**
 * Created by cry on 2015.11.06..
 */
trait CounterService {
  def addCounter(): Unit

  def subscribe(uid: Uid): Unit

  def increment(uid: Uid): Unit

  def decrement(uid: Uid): Unit
  
  def unsubscribe(uid: Uid): Unit
}

trait CounterState {
  val value: Int
  val hasPrev: Boolean
  val hasNext: Boolean
  val index: Int

  def next: Option[(Uid, CounterState)]

  def prev: Option[(Uid, CounterState)]
}

object CounterState {
  def apply(iValue: Int, iIndex: Int, iHasNext: Boolean, iHasPrev: Boolean, iNext: => Option[(Uid, CounterState)], iPrev: => Option[(Uid, CounterState)]): CounterState = new CounterState {
    override val value: Int = iValue
    override val hasPrev: Boolean = iHasPrev
    override val hasNext: Boolean = iHasNext
    override val index: Int = iIndex

    override def prev: Option[(Uid, CounterState)] = iPrev

    override def next: Option[(Uid, CounterState)] = iNext
  }
}

trait CounterStateAccessor {
  def firstAvailable(actual: Option[(Uid, CounterState)]): Option[(Uid, CounterState)]

  def apply(uid: Uid): Option[(Uid, CounterState)]
}

case class CountersChanged(f: CounterStateAccessor) extends Event


object CounterServiceImpl extends Behavior[CounterServiceImpl] {
//  def subscribe(uid: Uid) = Action { (context, model) =>
//    val newModel = model.copy(counters = model.counters.updated(uid, 0))
//    modelChanged(context, newModel)
//    newModel
//  }
//
//  def unsubscribe(uid: Uid) = Action { (context, model) =>
//    val newModel = model.copy(counters = model.counters - uid)
//    newModel
//  }
//
//  def decrement(uid: Uid) = Action { (context, model) =>
//    val newModel = model.copy(counters = model.counters.updated(uid, model.counters.getOrElse(uid, 0) - 1))
//    modelChanged(context, newModel)
//    newModel
//  }
//
//  def increment(uid: Uid) = Action { (context, model) =>
//    val newModel =  model.copy(counters = model.counters.updated(uid, model.counters.getOrElse(uid, 0) + 1))
//    modelChanged(context, newModel)
//    newModel
//  }
//
//  private[this] def modelChanged(context: BehaviorContext, newModel: CounterServiceImpl): Unit = {
//    context.onAction(CountersChanged(stateAccessor(newModel.counters)))
//  }
//
  private def stateAccessor(counters: ListMap[Uid, Int]): CounterStateAccessor = {
    val indexedCounters = counters.zipWithIndex.map { case ((uid, value), index) => uid ->(value, index) } toMap

    def createState(iUid: Uid): Option[(Uid, CounterState)] = for {
      (_, (value, index)) <- indexedCounters.find { case (uid, (value, index)) => uid == iUid }
    } yield {

        val oNextUid = indexedCounters.find { case (uid, (_, currIndex)) => currIndex == index + 1 }
        lazy val nextState = oNextUid.flatMap { case (uid, (_, _)) => createState(uid) }
        val oPrevUid = indexedCounters.find { case (uid, (_, currIndex)) => currIndex == index - 1 }
        lazy val prevState = oPrevUid.flatMap { case (uid, (_, _)) => createState(uid) }

        (iUid, CounterState(value,
          index,
          oNextUid.isDefined,
          oPrevUid.isDefined,
          nextState,
          prevState
        ))
      }

    new CounterStateAccessor {
      override def firstAvailable(actual: Option[(Uid, CounterState)]): Option[(Uid, CounterState)] = actual match {
        case None =>
          for {
            (uid, (_, _)) <- indexedCounters.find { case (_, (_, index)) => index == 0 }
            counterState <- createState(uid)
          } yield counterState
        case Some((_, counterState)) =>
          (for {
            (uid, (_, _)) <- indexedCounters.find { case (_, (_, index)) => index == counterState.index - 1  }
            newCounterState <- createState(uid)
          } yield newCounterState) match {
            case s: Some[_] => s
            case None => for {
              (uid, (_, _)) <- indexedCounters.find { case (_, (_, index)) => index == 0 }
              counterState <- createState(uid)
            } yield counterState
          }
      }

      override def apply(uid: Uid): Option[(Uid, CounterState)] = createState(uid)
    }
  }
  
//  val addCounter = action { (context, model) =>
//
//    CounterAdded(Uid())
//  }
}
trait Action extends Event
case class AddCounter(uid: Uid) extends Action

case class CounterServiceImpl(uid: Uid = Uid(), counters: ListMap[Uid, Int] = ListMap()) extends Service[CounterService, CounterServiceImpl] {


  override def handle(implicit context: UpdateContext): EventHandler[CounterServiceImpl] = EventHandler {
    case Init =>
      println("CounterService Init was called")
      this
    case AddCounter(uid) =>
      val newModel = copy(counters = counters.updated(uid, -1)) 
      context.onAction(CountersChanged(CounterServiceImpl.stateAccessor(newModel.counters)))
      newModel
  }

  override def api(context: UpdateContext): CounterService = new CounterService {
    override def subscribe(sUid: Uid): Unit = ??? //context.onAction(CounterServiceImpl.subscribe(sUid)(uid))

    override def decrement(sUid: Uid): Unit = ??? //context.onAction(CounterServiceImpl.decrement(sUid)(uid))

    override def increment(sUid: Uid): Unit = ??? //context.onAction(CounterServiceImpl.increment(sUid)(uid))

    override def unsubscribe(sUid: Uid): Unit = ??? //context.onAction(CounterServiceImpl.unsubscribe(sUid)(uid))
    
    override def addCounter(): Unit = context.onAction(AddCounter(Uid()))
  }
} 
