package com.chromy.frpui.core

import com.chromy.frpui.core.Updater.Simple
import monocle._

/**
 * Created by cry on 2015.10.18..
 */

trait Context {
  def getService[B <: Model[B] : Manifest](serviceName: String): B
  def onAction(action: Action): Unit
}


case class Child[M, B](lens: Lens[M, B]) {
  private[this] def updateSeq[B](action: Action, list: Seq[B])(implicit context: Context): Seq[B] = {
    list.map {
      case e: BaseModel => e.step(action).asInstanceOf[B]
      case e => e
    }
  }

  private[this] def updateMap[K, B](action: Action, map: Map[K, B])(implicit context: Context): Map[K, B] = {
    map.map {
      case (key, value: BaseModel) => (key, value.step(action).asInstanceOf[B])
      case e => e
    }
  }

  def step(action: Action, model: M)(implicit context: Context): M = {
    val newModel = lens.get(model) match {
      case m: BaseModel =>
        m.step(action)
      case s: Seq[B] =>
        updateSeq(action, s)
      case m: Map[_, B] =>
        updateMap(action, m)
      case e => e
    }
    lens.set(newModel.asInstanceOf[B])(model)
  }
}


trait Updater[B] {
  val handle: PartialFunction[Action, B]
}

object Updater {
  case class Simple[B](handle: PartialFunction[Action, B]) extends Updater[B]
}



object BaseModel {
  def step[A <: BaseModel](action: Action, model: A, children: List[Child[A, _]], handle: Updater[A])(implicit context: Context): A = {
    val handler = handle.handle
    val initialModel: A = if (handler.isDefinedAt(action)) handler.apply(action) else model.asInstanceOf[A]
    val newModel = children.foldLeft(initialModel) { (model, child) =>
      child.step(action, model)
    }
    newModel.asInstanceOf[A]
  }
}

trait BaseModel {
  type M <: BaseModel

  val uid: Uid

  def children: List[Child[M, _]] = Nil

  def step(action: Action)(implicit context: Context): M = BaseModel.step[M](action, this.asInstanceOf[M], children, handle)

  val handle: Updater[M] = Simple { case _ => this.asInstanceOf[M] }
}

trait Model[B <: BaseModel] extends BaseModel {
  type M = B
  implicit lazy val model: B = this.asInstanceOf[B]
}
