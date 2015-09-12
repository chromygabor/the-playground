package com.chromy.reactiveui.myjavafx

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.myjavafx.CounterApp.ServiceBuilder
import com.chromy.reactiveui.myjavafx.CounterStore._

import scala.collection.immutable.ListMap

/**
 * Created by chrogab on 2015.09.03..
 */
case class CounterStoreModel(counters: ListMap[Uid, Int] = ListMap(), uid: Uid = Uid()) extends Model[CounterStore]

object CounterStore {

  implicit object Builder extends ServiceBuilder[CounterStore] {
    private[this] var _instance = Option.empty[CounterStore]

    override def init(context: ContextMapper[CounterStoreModel]): () => CounterStore = {
      if (_instance.isEmpty) {
        _instance = Some(new CounterStore(context, CounterStoreModel()))
      }
      { () => _instance.get }
    }
  }

  trait CounterState {
    val value: Int
    val hasPrev: Boolean
    val hasNext: Boolean

    def next: Option[(Uid, CounterState)]

    def prev: Option[(Uid, CounterState)]
  }

  object CounterState {
    def apply(iValue: Int, iHasNext: Boolean, iHasPrev: Boolean, iNext: => Option[(Uid, CounterState)], iPrev: => Option[(Uid, CounterState)]): CounterState = new CounterState {
      override val value: Int = iValue
      override val hasPrev: Boolean = iHasPrev
      override val hasNext: Boolean = iHasNext

      override def prev: Option[(Uid, CounterState)] = iPrev

      override def next: Option[(Uid, CounterState)] = iNext
    }
  }

  trait CounterStateAccessor {
    def first: Option[(Uid, CounterState)]

    def apply(uid: Uid): Option[(Uid, CounterState)]
  }

  case class Changed(f: CounterStateAccessor) extends Action

}


class CounterStore(protected val contextMapper: ContextMapper[CounterStoreModel], protected val initialState: CounterStoreModel) extends Component[CounterStoreModel] {
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
          oNextUid.isDefined,
          oPrevUid.isDefined,
          nextState,
          prevState
        ))
      }

    new CounterStateAccessor {
      override def first: Option[(Uid, CounterState)] = for {
        (uid, (_, _)) <- indexedCounters.find { case (_, (_, index)) => index == 0 }
        counterState <- createState(uid)
      } yield counterState

      override def apply(uid: Uid): Option[(Uid, CounterState)] = createState(uid)
    }
  }

  private[this] def update(uid: Uid, f: Int => Int): Unit = {
    val update: (Uid, CounterStoreModel) => CounterStoreModel = {
      case (uid, model) =>
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
    }
    channel.onNext(Defer(uid, update))
  }

  def create(uid: Uid): Unit = {
    update(uid, _ + 0)
  }

  def increment(uid: Uid): Unit = {
    update(uid, _ + 1)
  }

  def decrement(uid: Uid): Unit = {
    update(uid, _ - 1)
  }

}
