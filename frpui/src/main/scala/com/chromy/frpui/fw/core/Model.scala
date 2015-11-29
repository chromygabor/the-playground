package com.chromy.frpui.fw.core

import com.typesafe.scalalogging.LazyLogging
import monocle._

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by cry on 2015.10.18..
 */

case object Init extends Event

case class Targeted(target: Uid, action: Event) extends Event

trait PostOrderAction

trait PreOrderAction

trait Initializable

case class Child[M, B](lens: Lens[M, B]) {

  private[this] def updateSeq[B](action: Event, prevList: Seq[B], list: Seq[B])(implicit context: UpdateContext): Seq[B] = {
    val prevSet = prevList.toSet

    list.map(input => input match {
      case e: BaseModel with Initializable if !prevSet(input) =>
        e.step(Init).step(action).asInstanceOf[B]
      case e: BaseModel => e.step(action).asInstanceOf[B]
      case e => e
    })
  }

  private[this] def updateMap[K, B](action: Event, prevMap: Map[K, B], map: Map[K, B])(implicit context: UpdateContext): Map[K, B] = {
    val prevSet = prevMap.values.toSet
    map.map { case (key, item) =>
      item match {
        case (value: BaseModel with Initializable) if (!prevSet(item)) =>
          (key, value.step(Init).step(action).asInstanceOf[B])
        case (value: BaseModel) => (key, value.step(action).asInstanceOf[B])
        case e => (key, e)
      }
    }
  }

  def step(action: Event, previousModel: M, model: M)(implicit context: UpdateContext): M = {
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

object BaseModel extends LazyLogging {

  import scala.concurrent.ExecutionContext.Implicits.global

  def step[A <: BaseModel](action: Event, model: A, children: List[Child[A, _]], handle: EventHandler[A])(implicit context: UpdateContext): A = {
    val previousModel = model

    val newModel = action match {
      case e: PostOrderAction =>
        val steppedModel = children.foldLeft(model) { (model, child) =>
          child.step(action, previousModel, model)
        }
        action match {
          case a: Action[_] if a.isDefinedAt(model) =>
            val action = a.asInstanceOf[Action[A]]
            logger.debug("s\"Receive Action: $action\"")
            val Result(newModel, nextAction) = action.apply(model, context)
            logger.debug(s"    stepping and returning $model -> $newModel")
            Future {
              logger.debug(s"    but in the meanwhile we are computing next action")
              nextAction(newModel, context)
            }.onComplete {
              case Success(event) =>
                logger.debug(s"    publishing event: $event")
                context.!(event)
              case Failure(error) =>
                error.printStackTrace()
            }
            newModel
          case a: Action[_] =>
            logger.debug(s"Action: $a -- $model")
            model
          case _ =>
            logger.debug(s"Stepping $model for $action")
            val handler = steppedModel.handle.handle
            if (handler.isDefinedAt(e)) handler.apply(e) else steppedModel.asInstanceOf[A]
        }
      case _ =>
        val initialModel: A = action match {
          case a: Action[_] if a.isDefinedAt(model) =>
            val action = a.asInstanceOf[Action[A]]
            val Result(newModel, nextAction) = action.apply(model, context)
            logger.debug(s"    stepping and returning $model -> $newModel")
            Future {
              logger.debug(s"    but in the meanwhile we are computing next action")
              nextAction(newModel, context)
            }.onComplete {
              case Success(event) =>
                logger.debug(s"    publishing event: $event")
                context.!(event)
              case Failure(error) =>
                error.printStackTrace()
            }
            newModel
          case a: Action[_] =>
            logger.debug(s"Action: $a -- $model")
            model
          case _ =>
            logger.debug(s"Stepping $model for $action")
            val handler = handle.handle
            if (handler.isDefinedAt(action)) handler.apply(action) else model.asInstanceOf[A]
        }
        children.foldLeft(initialModel) { (model, child) =>
          child.step(action, previousModel, model)
        }
    }
    newModel.asInstanceOf[A]
  }
}

trait BaseModel {
  type M <: BaseModel

  val uid: Uid

  protected def children: List[Child[M, _]] = Nil

  final def step(action: Event)(implicit context: UpdateContext): M = BaseModel.step[M](action, this.asInstanceOf[M], children, handle(context))

  protected[core] def handle(implicit context: UpdateContext): EventHandler[M] = EventHandler()
}

trait Model[B <: BaseModel] extends BaseModel with Initializable {
  type M = B
}
