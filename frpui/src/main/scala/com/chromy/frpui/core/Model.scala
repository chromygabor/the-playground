package com.chromy.frpui.core

import com.chromy.frpui.core.BaseModel.Defer
import monocle._

/**
 * Created by cry on 2015.10.18..
 */

case object Init extends Action
case class Targeted(target: Uid, action: Action) extends Action

trait Initializable

trait Context {
  def getService[B : Manifest]: B

  def onAction(action: Action): Unit
}


case class Child[M, B](lens: Lens[M, B]) {

  private[this] def updateSeq[B](action: Action, prevList: Seq[B], list: Seq[B])(implicit context: Context): Seq[B] = {
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

  private[this] def updateMap[K, B](action: Action, prevMap: Map[K, B], map: Map[K, B])(implicit context: Context): Map[K, B] = {
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

  def step(action: Action, previousModel: M, model: M)(implicit context: Context): M = {
    val newModel = lens.get(model) match {
      case m: BaseModel =>
        action match {
          case d: Defer[M] if m.uid == d.uid =>
            d.f(context, m.asInstanceOf[M])
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


trait Updater[+B] {
  val handle: PartialFunction[Action, B]
}

object Updater {
  def apply[B](iHandle: PartialFunction[Action, B]) = new Updater[B] { override val handle: PartialFunction[Action, B] = iHandle }
  def apply() = new Updater[Nothing] {
    override val handle: PartialFunction[Action, Nothing] = new PartialFunction[Action, Nothing] {
      override def isDefinedAt(x: Action): Boolean = false

      override def apply(v1: Action): Nothing = ???
    }
  }
}


object BaseModel {
  case class Defer[M](uid: Uid, f: (Context, M) => M) extends Action

  def step[A <: BaseModel](action: Action, model: A, children: List[Child[A, _]], handle: Updater[A])(implicit context: Context): A = {
    val handler = handle.handle
    val previousModel = model
    val initialModel: A = if (handler.isDefinedAt(action)) handler.apply(action) else model.asInstanceOf[A]
    val newModel = children.foldLeft(initialModel) { (model, child) =>
      child.step(action, previousModel, model)
    }
    newModel.asInstanceOf[A]
  }
}

trait BaseModel {
  type M <: BaseModel

  val uid: Uid

  protected def children: List[Child[M, _]] = Nil

  def step(action: Action)(implicit context: Context): M = BaseModel.step[M](action, this.asInstanceOf[M], children, handle(context))

  protected def handle(implicit context: Context): Updater[M]

  def defer(f: (Context, M) => M)(implicit context: Context): Unit = {
    context.onAction(Defer(uid, f))
  }
  
}

trait Model[B <: BaseModel] extends BaseModel with Initializable {
  type M = B
}
