package com.chromy.reactiveui.myjavafx

import com.chromy.reactiveui.core._

import scala.util.{Success, Try}

/**
 * Created by chrogab on 2015.09.03..
 */
case class CounterStoreModel(counters: Map[Uid, Int] = Map(), uid: Uid = Uid()) extends Model[CounterStore]

class CounterStore(protected val contextMapper: ContextMapper[CounterStoreModel], protected val initialState: CounterStoreModel) extends Component[CounterStoreModel] {
  override protected def upd(model: ModelType) = {
    case _ =>
      model
  }

  def increment(uid: Uid): Unit = {
    val update: (Try[Uid], CounterStoreModel) => CounterStoreModel = { case(Success(uid), model) =>
      val newValue = if(model.counters.contains(uid)) {
        model.counters(uid) + 1
      } else {
        0
      }
      model.copy(counters = model.counters.updated(uid, newValue))
    }
    channel.onNext(Defer(Success(uid), update))
  }
}
