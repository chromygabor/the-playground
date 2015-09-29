package com.chromy.reactiveui.myjavafx

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.myjavafx.CounterStore._
import rx.lang.scala.Subscriber
import com.chromy.reactiveui._

import scala.collection.immutable.ListMap
import scala.concurrent.Future

/**
 * Created by chrogab on 2015.09.03..
 */

object CounterStore {

  case class Model(counters: ListMap[Uid, Int] = ListMap(), uid: Uid = Uid()) extends core.Model[CounterStore]

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

  case class Changed(f: CounterStateAccessor) extends Action

}


class CounterStore(protected val contextMapper: ContextMapper[CounterStore.Model]) extends Component[CounterStore.Model] {
  private[this] def stateAccessor(counters: ListMap[Uid, Int]): CounterStateAccessor = {
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

  private[this] def update(uid: Option[Uid], f: Int => Int): Unit = {
    val update: (Option[Uid], CounterStore.Model) => CounterStore.Model = {
      case (Some(uid), model) =>
        val newCounters = if (model.counters.contains(uid)) {
          val newValue = f(model.counters(uid))
          model.counters.map { case (iUid, iValue) =>
            if (uid == iUid) iUid -> newValue
            else iUid -> iValue
          }
        } else {
          model.counters.updated(uid, 0)
        }
        channel.onNext(CounterStore.Changed(stateAccessor(newCounters)))
        model.copy(counters = newCounters)
      case (None, model) =>
        channel.onNext(CounterStore.Changed(stateAccessor(model.counters)))
        model
    }

    channel.onNext(Defer(uid, update))
  }

  def create(uid: Uid): Unit = {
    update(Some(uid), _ + 0)
  }

  def increment(uid: Uid): Unit = {
    update(Some(uid), _ + 1)
  }

  def decrement(uid: Uid): Unit = {
    update(Some(uid), _ - 1)
  }

  def refresh(): Unit = {
    update(None, _ + 0  )
  }

  def remove(uid: Uid) = {
    val update: (Uid, CounterStore.Model) => CounterStore.Model = { case (uid, model) =>

      val newCounters = model.counters - uid

      channel.onNext(CounterStore.Changed(stateAccessor(newCounters)))
      model.copy(counters = newCounters)
    }
    channel.onNext(Defer(uid, update))

  }


}
