package com.chromy.reactiveui

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.TestUtils._
import com.chromy.reactiveui.core.misc.{Executable}
import com.chromy.reactiveui.core.{Action, Uid}
import monocle.Lens
import monocle.macros.GenLens
import rx.lang.scala.schedulers.{IOScheduler, ComputationScheduler, ImmediateScheduler}
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
    private[this] val _subscribers: MMap[(T => Executable), Int] = WMap()
    private[this] val _lastEmittedItem = new AtomicReference[T]()
    val subscribeQueue: ConcurrentLinkedQueue[Executable]

    val name: String
    val uid: Uid

    private[this] def subscribers: List[(T => Executable)] = {
      var newSubscribers: List[T => Executable] = null
      _subscribersLock.synchronized {
        newSubscribers = _subscribers.toList.sortBy(_._2).map {
          _._1
        }
      }
      newSubscribers
    }

    def subscribe(subscriber: (T) => Executable): Unit = {
      _subscribersLock.synchronized {
        _subscribers.update(subscriber, _subscribers.size)
      }

      if(_lastEmittedItem.get() != null) {
        val e = subscriber(_lastEmittedItem.get())
        subscribeQueue offer e
      }
    }

    def createRenderer(in: T): Executable = {
      if(in != _lastEmittedItem.getAndSet(in)) {
        println(s"[CREATING RENDERER] $name[$uid]: $in")
        _lastEmittedItem.set(in)
        subscribers.foldLeft(Executable()) { case (executables, subscriber) =>
          val s = { () => subscriber(in) }
          executables.append(s)
        }
      } else Executable()
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

    def toMapCtx[K,V](f: (MapOperation[K, V]) => Ctx[V])(implicit ev: B <:< Map[K, V]): Ctx[Map[K, Ctx[V]]] = {
      new MapCtx[Map[K, V], K, V](apply().asInstanceOf[Ctx[Map[K, V]]]) {
        override val getContext: (MapOperation[K, V]) => Ctx[V] = f
        override val subscribeQueue: ConcurrentLinkedQueue[Executable] = parent.subscribeQueue
      }.asInstanceOf[Ctx[Map[K, Ctx[V]]]]
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
    override val subscribeQueue: ConcurrentLinkedQueue[Executable] = parent.subscribeQueue

    parent.react { (action, originalModel, model) =>
      val newModel = update(action, lens.get(originalModel), lens.get(model))
      lastItem.set(newModel)
      lens.set(newModel)(model)
    }

    parent.subscribe { in => createRenderer(lens.get(in)) }
  }

  /**
   * Context which can handle a map
   * @param parent
   * @tparam B
   * @tparam V
   */
  abstract class MapCtx[B <: Map[K, V], K, V](parent: Ctx[B]) extends Ctx[Map[K, Ctx[V]]] {
    override val name: String = "MapCtx"

    val getContext: MapOperation[K, V] => Ctx[V]

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
        override val subscribeQueue: ConcurrentLinkedQueue[Executable] = parent.subscribeQueue
      }
    }

    private[this] def reactor(action: Action, original: B, model: B): B = {
      diff(original, model) map { case (action, uid) =>
        action match {
          case Added => uid -> getContext(Add(uid, createContext(uid, model(uid))))
          case Removed => uid -> getContext(Remove(uid))
        }
      }

      val newValues = model.map { case (key, value) =>
        val ctx = getContext(Get[K, V](key))
        val originalValue = original.getOrElse(key, value)
        (key, ctx.update(action, originalValue, value), ctx)
      }

      lastItem.set(newValues.map { case (key, _, ctx) => key -> ctx }.toMap)
      newValues.map { case (key, newValue, _) => key -> newValue }.toMap.asInstanceOf[B]
    }

    parent.react(reactor)

    parent.subscribe { model =>
      val m = model.map { case (uid, _) =>
        uid -> getContext(Get[K, V](uid))
      }

      m.foldLeft(createRenderer(m)) { case (executables, (uid, ctx)) =>
        if(model.contains(uid)) {
          executables.append{() => ctx.createRenderer(model(uid))}
        } else executables
      }
    }
  }


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

  val contexts = MMap[String, Ctx[Int]]()
  val subscribers = WMap[Ctx[Int], Int => Executable]()

  val ctx = ActiveCtx(MapModel(), ComputationScheduler(), IOScheduler())
  ctx.react(updateMap)

  ctx.subscribe { model =>
    Executable {
      println(s"[RENDER] ctx: $model")
    }
  }

  val mapCtx = ctx.newBy(GenLens[MapModel](_.values)).toMapCtx[String, Int] {
    case Add(uid, context) =>
      contexts.update(uid, context)
      context.react(updatePart(uid))
      context
    case Get(uid) =>
      contexts(uid)
    case Remove(uid) =>
      val context = contexts(uid)
      contexts.remove(uid)
      context
  }

  def subscriberFor(partId: String): Int => Executable = { input =>
    Executable {
      println(s"[RENDER] mapCtxChildren $partId: $input")
    }
  }

  val subscriber: Map[String, Ctx[Int]] => Executable = { actualMapOfCtx =>
    val executables = actualMapOfCtx.map { case (uid, ctx) =>
      if (!subscribers.contains(ctx)) {
        val subscriber = subscriberFor(uid)
        subscribers.update(ctx, subscriber)
        Executable {
          ctx.subscribe(subscriber)
          println(s"[RENDER] mapCtx: subscribing to $ctx")
        }
      } else Executable {}
    }
    Executable {
      executables.foreach(_.run())
    }
  }

  mapCtx.subscribe(subscriber)

  val partId1 = "partid1"
  ctx ! AddParticipant(partId1)
  Thread.sleep(100)

  ctx ! IncParticipant(partId1)
  Thread.sleep(100)

  ctx ! IncParticipant(partId1)
  Thread.sleep(100)

  val partId2 = "partid2"
  ctx ! AddParticipant(partId2)
  Thread.sleep(100)

  ctx ! IncParticipant(partId1)
  Thread.sleep(100)

  ctx ! IncParticipant(partId2)
  Thread.sleep(100)

  ctx ! RemoveParticipant(partId1)

  ctx ! IncParticipant(partId1)
  Thread.sleep(100)

  ctx ! IncParticipant(partId2)
  Thread.sleep(100)


  Thread.sleep(1000)
  /**
   * ActiveCtx
   */
  object ActiveCtx {
    def apply[A](initialState: A, compScheduler: Scheduler, ioScheduler: Scheduler): Ctx[A] = new Ctx[A] {
      override val name: String = "ActiveCtx"

      val lastItem = new AtomicReference[A](initialState)

      private[this] val s = Subject[Action]
      val subscribeQueue = new ConcurrentLinkedQueue[Executable]()

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

        Executable {
          res.run()
          while (!subscribeQueue.isEmpty) {
            subscribeQueue.poll().run()
          }
        }
      }.observeOn(ioScheduler).subscribe(new Subscriber[Executable]() {
        override def onNext(sideEffect: Executable): Unit = {
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


