package com.chromy.frpui.fw.core

import monocle._

/**
 * Created by cry on 2015.10.18..
 */

case object Init extends Event
case class Targeted(target: Uid, action: Event) extends Event
case class DelayedEvent[M](uid: Uid, f: (Context, M) => M) extends Event

trait Initializable

case class Child[M, B](lens: Lens[M, B]) {

  private[this] def updateSeq[B](action: Event, prevList: Seq[B], list: Seq[B])(implicit context: Context): Seq[B] = {
    val prevSet = prevList.toSet

    list.map { input =>
      input match {
        case e: BaseModel with Initializable if (!prevSet(input)) =>
          e.step(Init).step(action).asInstanceOf[B]
        case e: BaseModel => e.step(action).asInstanceOf[B]
        case e => e
      }
    }
  }

  private[this] def updateMap[K, B](action: Event, prevMap: Map[K, B], map: Map[K, B])(implicit context: Context): Map[K, B] = {
    val prevSet = prevMap.values.toSet
    map.map { case(key, item) => 
      item match {
        case (value: BaseModel with Initializable) if(!prevSet(item)) => 
          (key, value.step(Init).step(action).asInstanceOf[B])
        case (value: BaseModel) => (key, value.step(action).asInstanceOf[B])
        case e => (key, e)
      }
    }
  }

  def step(action: Event, previousModel: M, model: M)(implicit context: Context): M = {
    val newModel = lens.get(model) match {
      case m: BaseModel =>
        action match {
          case Targeted(m.uid, action) => m.step(action)
          case _ => m.step(action) 
        }
      case s: Seq[B] =>
        val ps = lens.get(previousModel).asInstanceOf[s.type]
        updateSeq(action, ps, s)
      case m: Map[_, B] =>
        val pm = lens.get(previousModel).asInstanceOf[m.type]
        updateMap(action, pm, m)
      case e => e
    }

    lens.set(newModel.asInstanceOf[B])(model)
  }
}

object BaseModel {
  def step[A <: BaseModel](action: Event, model: A, children: List[Child[A, _]], handle: EventHandler[A])(implicit context: Context): A = {
    action match {
      case d: DelayedEvent[_] if d.uid == model.uid =>
        val defer = d.asInstanceOf[DelayedEvent[A]]
        defer.f(context, model)
      case _ =>
        val handler = handle.handle
        val previousModel = model

        val initialModel: A = if (handler.isDefinedAt(action)) handler.apply(action) else model.asInstanceOf[A]
        val newModel = children.foldLeft(initialModel) { (model, child) =>
          child.step(action, previousModel, model)
        }
        newModel.asInstanceOf[A]
    }
  }
}

trait BaseModel {
  type M <: BaseModel

  val uid: Uid

  protected def children: List[Child[M, _]] = Nil

  def step(action: Event)(implicit context: Context): M = BaseModel.step[M](action, this.asInstanceOf[M], children, handle(context))

  protected def handle(implicit context: Context): EventHandler[M] = EventHandler()

  //protected def action(f: (Context, M) => M): DelayedEvent[M] = DelayedEvent[M](uid, f)
}

trait Model[B <: BaseModel] extends BaseModel with Initializable {
  type M = B
}
