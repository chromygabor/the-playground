package com.chromy.reactiveui

import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.TestUtils._
import com.chromy.reactiveui.core.misc.{SideEffect, SideEffect$}
import com.chromy.reactiveui.core.{Action, Uid}
import monocle.Lens
import monocle.macros.GenLens
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Scheduler, Subject, Subscriber}

import scala.collection.mutable.{Map => MMap, WeakHashMap => WMap}


/**
 * Created by cry on 2015.07.12..
 */

object TestUtils {

  case class AddNumber(number: Int) extends Action

  case class MapModel(values: Map[String, Int] = Map())

  case class AddParticipant(partId: String) extends Action
  case class RemoveParticipant(partId: String) extends Action

  case class IncParticipant(partId: String) extends Action

}

object CtxTest extends App {

  sealed trait MapOperation[Uid, V]

  case class Add[Uid, V](uid: Uid, context: Ctx[V]) extends MapOperation[Uid, V]

  case class Remove[Uid, V](uid: Uid) extends MapOperation[Uid, V]

  case class Get[Uid, V](uid: Uid) extends MapOperation[Uid, V]

  trait CtxReactor[A] {
    val name: String
    val uid: Uid
    /**
     * Subscribers
     */
    private[this] val _reactorsLock: Object = new Object()
    private[this] val _reactors: WMap[((Action, A, A) => A), Int] = WMap()
    val lastItem: AtomicReference[A]
    private[this] def reactors: List[(Action, A, A) => A] = {
      var newReactors:List[((Action, A, A) => A)]  = null
      _reactorsLock.synchronized {
        newReactors = _reactors.toList.sortBy(_._2).map {
          _._1
        }
      }
      newReactors
    }
    def react(subscriber: (Action, A, A) => A): Unit = {
      _reactorsLock.synchronized {
        _reactors.update(subscriber, _reactors.size)
      }
    }

    def update(action: Action, originalModel: A, model: A): A = {
      val res = reactors match {
        case Nil => model
        case h :: Nil => h(action, originalModel, model)
        case h :: t =>
          t.foldLeft(h(action, originalModel, model)) { (accu, act) =>
            act(action, originalModel, accu)
          }
      }
      lastItem.set(res)
      println(s"[UPDATE] $name[$uid] from $model to $res")
      res
    }
  }

  trait CtxRender[T] {
    private[this] val _subscribersLock: Object = new Object()
    private[this] val _subscribers: MMap[(T => SideEffect), Int] = WMap()
    private[this] val _lastEmittedItem = new AtomicReference[T]()

    val name: String
    val uid: Uid

    private[this] def subscribers: List[(T => SideEffect)] = {
      var newSubscribers: List[T => SideEffect] = null
      _subscribersLock.synchronized {
        newSubscribers = _subscribers.toList.sortBy(_._2).map {
          _._1
        }
      }
      newSubscribers
    }

    def subscribe(subscriber: (T) => SideEffect): SideEffect = {
      _subscribersLock.synchronized {
        _subscribers.update(subscriber, _subscribers.size)
      }
      if(_lastEmittedItem.get != null) {
        subscriber.apply(_lastEmittedItem.get)
      } else SideEffect()
    }

    def createRenderer(in: T): SideEffect = {
      if(in != _lastEmittedItem.getAndSet(in)) {
        println(s"[CREATING RENDERER] $name[$uid]: $in")
        _lastEmittedItem.set(in)
        subscribers.foldLeft(SideEffect()) { case (executables, subscriber) =>
          val s = { () => subscriber(in) }
          executables.append(s)
        }
      } else SideEffect()
    }
  }

  trait Ctx[T] extends CtxReactor[T] with CtxRender[T]{
    val uid: Uid = Uid()
    val name: String

    def !(action: Action) = onAction(action)

    def onAction(action: Action): Unit

    def newBy[B](lens: Lens[T, B]): CtxApplier[T, B] = new CtxApplier[T, B](this, lens)

    override def toString() = {
      s"$name[$uid]: ${lastItem.get}"
    }

    override def hashCode(): Int = uid.uid

    override def equals(obj: scala.Any): Boolean = obj match {
      case e: Ctx[T] => e.uid == uid
      case _ => false
    }
  }


  class CtxApplier[A, B](parent: Ctx[A], lens: Lens[A, B]) {
    def apply(): Ctx[B] = new LensCtx(parent, lens)

    def toMapCtx[K,V](f: (MapOperation[K, V]) => Ctx[V])(implicit ev: B <:< Map[K, V]): Ctx[Map[K, Ctx[V]]] = new MapCtx[K, V](apply().asInstanceOf[Ctx[Map[K, V]]], f)

    def toSeqCtx[V]()(implicit ev: B <:< Seq[V]): Ctx[Ctx[Seq[V]]] = new SeqCtx[V](apply().asInstanceOf[Ctx[Seq[V]]])
  }

  /**
   *
   * @param parent
   * @tparam V
   */
  class SeqCtx[V](parent: Ctx[Seq[V]]) extends Ctx[Ctx[Seq[V]]] {

    trait MapAction
    case object Added extends MapAction
    case object Removed extends MapAction


    override val name: String = "SeqCtx"

    override def onAction(action: Action): Unit = parent onAction action

    override val lastItem: AtomicReference[Ctx[Seq[V]]] = new AtomicReference[Ctx[Seq[V]]]()

    private def diff(left: Seq[V], right: Seq[V]): List[(MapAction, V)] = {

      val l1i = left.zipWithIndex.foldLeft(Map.empty[V, List[Int]]) { case (map, (elem, index)) =>
        map.updated(elem, index :: map.getOrElse(elem, Nil))
      }

      val l2i = right.zipWithIndex.foldLeft(Map.empty[V, List[Int]]) { case (map, (elem, index)) =>
        map.updated(elem, index :: map.getOrElse(elem, Nil))
      }

      val union = left.union(right).toSet

      union.foldLeft(List.empty[(MapAction, V)]) {case (list, elem) =>
        (l1i.get(elem), l2i.get(elem)) match {
          case (Some(_), None) =>
            (Removed, elem) :: list
          case (None, Some(_)) =>
            (Added, elem) :: list
          case (Some(_), Some(_)) =>
            list
        }
      }
    }

    private[this] val parentReactor: (Action, Seq[V], Seq[V]) => Seq[V] = { (action, original, model) =>
      ???
    }

  }

  /**
   * Simple ctx which was mapped by a lens
   * @param parent
   * @param lens
   * @tparam A
   * @tparam B
   */
  class LensCtx[A, B](parent: Ctx[A], lens: Lens[A, B]) extends Ctx[B] {
    override val name: String = "LensCtx"
    override def onAction(action: Action): Unit = parent onAction action
    override val lastItem: AtomicReference[B] = new AtomicReference[B](lens.get(parent.lastItem.get()))

    private[this] val parentReactor: (Action, A, A) => A = { (action, originalModel, model) =>
      val newModel = update(action, lens.get(originalModel), lens.get(model))
      lastItem.set(newModel)
      lens.set(newModel)(model)
    }
    private[this] val parentSubscriber: A => SideEffect = { in => createRenderer(lens.get(in)) }

    parent.react(parentReactor)
    parent.subscribe(parentSubscriber)
  }

  /**
   * Context which can handle a map
   * @param parent
   * @tparam V
   */
  class MapCtx[K, V](parent: Ctx[Map[K, V]], getContext: MapOperation[K, V] => Ctx[V]) extends Ctx[Map[K, Ctx[V]]] {
    override val name: String = "MapCtx"

    override val lastItem: AtomicReference[Map[K, Ctx[V]]] = new AtomicReference[Map[K, Ctx[V]]]()

    override def onAction(action: Action): Unit = parent onAction action

    trait MapAction
    case object Added extends MapAction
    case object Removed extends MapAction

    private[this] def diff(left: Map[K, V], right: Map[K, V]): List[(MapAction,K)] = {
      if (left != right) {
        right.foldLeft(List[(MapAction,K)]()) { case (accu, (partId, value)) =>
          left.contains(partId) match {
            case true => accu
            case false => (Added, partId) :: accu
          }
        } ++ left.foldLeft(List[(MapAction,K)]()) { case (accu, (partId, value)) =>
          right.contains(partId) match {
            case true => accu
            case false => (Removed, partId) :: accu
          }
        }
      } else List()
    }


    private[this] def createContext(partId: K, initialState: V): Ctx[V] = {
      val parent = this
      new Ctx[V] {
        override val name: String = "MapCtxChildren"
        override def onAction(action: Action): Unit = parent onAction action
        override val lastItem: AtomicReference[V] = new AtomicReference(initialState)
      }
    }

    private[this] val parentReactor: (Action, Map[K, V], Map[K, V]) =>  Map[K, V] = { (action: Action, original: Map[K, V], model: Map[K, V]) =>
      diff(original, model) map { case (mapAction, key) =>
        mapAction match {
          case Added => key -> getContext(Add(key, createContext(key, model(key))))
          case Removed => key -> getContext(Remove(key))
        }
      }

      val newValues = model.map { case (key, value) =>
        val ctx = getContext(Get[K, V](key))
        val originalValue = original.getOrElse(key, value)
        (key, ctx.update(action, originalValue, value), ctx)
      }

      lastItem.set(newValues.map { case (key, _, ctx) => key -> ctx }.toMap)
      newValues.map { case (key, newValue, _) => key -> newValue }.toMap.asInstanceOf[Map[K, V]]
    }

    private[this] val parentSubscriber: (Map[K, V] => SideEffect) = { model =>
      val m = model.map { case (uid, _) =>
        uid -> getContext(Get[K, V](uid))
      }

      m.foldLeft(createRenderer(m)) { case (executables, (uid, ctx)) =>
        if(model.contains(uid)) {
          executables.append{() => ctx.createRenderer(model(uid))}
        } else executables
      }
    }

    parent.react(parentReactor)

    parent.subscribe(parentSubscriber)
  }


  val contexts = MMap[String, Ctx[Int]]()
  val subscribers = WMap[Ctx[Int], Int => SideEffect]()
  val reactors = WMap[Ctx[Int], (Action, Int, Int) => Int]()

  val updateMap: (Action, MapModel, MapModel) => MapModel = { (action, original, model) =>
    action match {
      case AddParticipant(partId) => model.copy(values = model.values.updated(partId, 0))
      case RemoveParticipant(partId) => model.copy(values = model.values - partId)
      case _ => model
    }
  }

  def updatePart(partId: String): (Action, Int, Int) => Int = { (action, original, model) =>
    action match {
      case IncParticipant(uid) if uid == partId => model + 1
      case _ => model
    }
  }

  val ctx = ActiveCtx(MapModel(), ComputationScheduler(), ComputationScheduler())
  ctx.react(updateMap)

  ctx.subscribe { model =>
    SideEffect {
      //println(s"[RENDER] ctx: $model")
    }
  }

  val mapCtx = ctx.newBy(GenLens[MapModel](_.values)).toMapCtx[String, Int] {
    case Add(uid, context) =>
      contexts.update(uid, context)
      val updater = updatePart(uid)
      reactors.update(context, updater)
      context.react(updater)
      context
    case Get(uid) =>
      contexts(uid)
    case Remove(uid) =>
      val context = contexts(uid)
      contexts.remove(uid)
      context
  }

  def subscriberFor(partId: String): Int => SideEffect = { input =>
    SideEffect {
      println(s"[RENDER] mapCtxChildren $partId: $input")
    }
  }

  val subscriber: Map[String, Ctx[Int]] => SideEffect = { actualMapOfCtx =>
    val executables = actualMapOfCtx.map { case (uid, ctx) =>
      if (!subscribers.contains(ctx)) {
        val subscriber = subscriberFor(uid)
        subscribers.update(ctx, subscriber)
        ctx.subscribe(subscriber)
          //println(s"[RENDER] mapCtx: subscribing to $ctx")
      } else SideEffect {}
    }
    SideEffect {
      executables.foreach(_.run())
    }
  }

  mapCtx.subscribe(subscriber)

  val partId1 = "partid1"
  ctx ! AddParticipant(partId1)
  val partId2 = "partid2"
  ctx ! AddParticipant(partId2)

  for(i <- 1 to 1000) {
    ctx ! IncParticipant(partId1)
    Thread.sleep(10)
    ctx ! IncParticipant(partId2)
    Thread.sleep(10)
  }

  /**
   * ActiveCtx
   */
  object ActiveCtx {
    def apply[A](initialState: A, compScheduler: Scheduler, ioScheduler: Scheduler): Ctx[A] = new Ctx[A] {
      override val name: String = "ActiveCtx"

      val lastItem = new AtomicReference[A](initialState)

      private[this] val s = Subject[Action]

      s.observeOn(compScheduler).map { action =>
        println(s"======================== Action: $action =================")
        val oldState = lastItem.get
        val newState = update(action, oldState, oldState)
        lastItem.set(newState)
        (oldState, newState)
      }.observeOn(compScheduler).map { case (oldState, newState) =>
        val res = createRenderer(newState)

        if (!res.errors.isEmpty) {
          res.errors.foreach(_.printStackTrace())
        }
        res
      }.observeOn(ioScheduler).subscribe(new Subscriber[SideEffect]() {
        override def onNext(sideEffect: SideEffect): Unit = {
          sideEffect.run()
        }

        override def onError(error: Throwable): Unit = super.onError(error)

        override def onCompleted(): Unit = super.onCompleted()
      })

      override def onAction(action: Action) = {
        s.onNext(action)
      }
    }
  }

}


