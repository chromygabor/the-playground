package com.chromy.reactiveui

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.TestUtils._
import com.chromy.reactiveui.core.misc.{Executable, SideChain}
import com.chromy.reactiveui.core.{Uid, Action, UpdateChain}
import monocle.Lens
import monocle.macros.GenLens
import rx.lang.scala.schedulers.ImmediateScheduler
import rx.lang.scala.{Scheduler, Subject, Subscriber}

import scala.collection.mutable.{Map => MMap}


/**
 * Created by cry on 2015.07.12..
 */

object TestUtils {

  case class SubSubModel(value: Int = 0)

  case class SimpleModel(sub: SubSubModel = SubSubModel())


  case class AddNumber(number: Int) extends Action

  case class MapModel(values: Map[Uid, Int] = Map())

  case class AddParticipant(partId: Uid) extends Action

  case class IncParticipant(partId: Uid) extends Action

}

object CtxTest extends App {
  sealed trait MapOperation[Uid,V]

  case class Add[Uid,V](uid: Uid, context: Ctx[V]) extends MapOperation[Uid,V]

  case class Remove[Uid,V](uid: Uid) extends MapOperation[Uid,V]

  case class Get[Uid,V](uid: Uid) extends MapOperation[Uid,V]

  trait Ctx[A] {

    final val uid: Uid = Uid()

    def !(action: Action) = onAction(action)

    def onAction(action: Action): Unit

    def subscribe(subscriber: A => Executable): Unit

    def addReactor(f: (Action, A, A) => A): Unit = chain.subscribe(f)

    def chain: UpdateChain[A]

    def render: SideChain[A]

    def lastItem: AtomicReference[A]

    def newBy[B](lens: Lens[A, B]): CtxApplier[A, B] = new CtxApplier[A, B](this, lens)

  }

  type MapCtxModel[Uid,V] = Map[Uid, Ctx[V]]


  class CtxApplier[A, B](parent: Ctx[A], lens: Lens[A, B]) {
    def apply(): Ctx[B] = new LensCtx(parent, lens)

    def toMapCtx[Uid, V](f: (MapOperation[Uid, V]) => Ctx[V])(implicit ev: B <:< Map[Uid, V]): Ctx[MapCtxModel[Uid,V]] = {
      new MapCtx[Map[Uid, V], Uid, V](apply().asInstanceOf[Ctx[Map[Uid,V]]]) {
        override val getContext: (MapOperation[Uid, V]) => Ctx[V] = f
      }.asInstanceOf[Ctx[MapCtxModel[Uid,V]]]
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
    override def onAction(action: Action): Unit = parent onAction action

    override val chain: UpdateChain[B] = parent.chain.map(lens)
    override val lastItem: AtomicReference[B] = new AtomicReference[B](lens.get(parent.lastItem.get()))

    override def render: SideChain[B] = parent.render.map { actual =>
      lens.get(actual)
    }.asBehavior(lens.get(parent.lastItem.get()))

    override def subscribe(subscriber: B => Executable): Unit = {
      render.subscribe(subscriber)
    }
  }

  /**
   * Context which can handle a map
   * @param parent
   * @tparam B
   * @tparam Uid
   * @tparam V
   */


  abstract class MapCtx[B <: Map[Uid, V], Uid, V](parent: Ctx[B]) extends Ctx[MapCtxModel[Uid,V]] {
    val getContext: MapOperation[Uid,V] => Ctx[V]

    override val lastItem: AtomicReference[MapCtxModel[Uid,V]] = new AtomicReference[MapCtxModel[Uid,V]]()
    override def chain: UpdateChain[MapCtxModel[Uid,V]] = throw new IllegalAccessException("This context doesn't have update chain")

    override val render: SideChain[MapCtxModel[Uid,V]] = SideChain[MapCtxModel[Uid,V]]()

    override def subscribe(subscriber: MapCtxModel[Uid,V] => Executable): Unit = render.subscribe(subscriber)

    override def onAction(action: Action): Unit = parent onAction action

    private[this] def diff(left: Map[Uid, V], right: Map[Uid, V]): List[Uid] = {
      if (left != right) {
        right.foldLeft(List[Uid]()) { case (accu, (partId, value)) =>
          left.contains(partId) match {
            case true => accu
            case false => partId :: accu
          }
        }
      } else List()
    }


    private[this] def createContext(iUid: Uid, initialState: V): Ctx[V] = {
      val parent = this
      new Ctx[V] {
        override def onAction(action: Action): Unit = parent onAction action

        override val chain: UpdateChain[V] = UpdateChain()

        override def lastItem: AtomicReference[V] = new AtomicReference(initialState)

        override def subscribe(subscriber: V => Executable): Unit = {
          render.subscribe(subscriber)
        }

        override def render: SideChain[V] = parent.render.map {in =>
          in(uid).lastItem.get
        }
      }
    }

    private[this] val update: (Action, B, B) => B = { (action, original, model) =>

      diff(original, model) map { uid =>
        uid -> getContext(Add(uid, createContext(uid, model(uid))))
      }

      val newValues = model.map { case (key, value) =>
        val ctx = getContext(Get[Uid,V](key))
        (key, ctx.chain.update(action, value), ctx)
      }

      lastItem.set(newValues.map { case (key, _, ctx) => key -> ctx}.toMap)
      newValues.map { case (key, newValue, _) => key -> newValue}.toMap.asInstanceOf[B]
    }
    parent.chain.subscribe(update)

    parent.render.subscribe { model =>
      val m = model.map { case (uid, _) =>
        uid -> getContext(Get[Uid,V](uid))
      }
      render.update(m)
    }
  }


  val updateMap: (Action, MapModel, MapModel) => MapModel = { (action, original, model) =>
    println(s"[UPDATE] updateMap: $action, $model")
    action match {
      case AddParticipant(uid) => model.copy(values = model.values.updated(uid, 0))
      case _ => model
    }
  }

  def updatePart(partId: Uid): (Action, Int, Int) => Int = { (action, original, model) =>
    println(s"[UPDATE] updatePart for $partId: $action, $model")
    action match {
      case IncParticipant(uid) if uid == partId => model + 1
      case _ => model
    }
  }

  val contexts = MMap[Uid, Ctx[Int]]()
  val subscribers = MMap[Uid, Int => Executable]()

  val ctx = ActiveCtx(MapModel(), ImmediateScheduler(), ImmediateScheduler())
  ctx.addReactor(updateMap)

  ctx.subscribe { model =>
    Executable {
      println(s"[RENDER] ctx: $model")
    }
  }

  val mapCtx = ctx.newBy(GenLens[MapModel](_.values)).toMapCtx[Uid, Int] {
    case Add(uid, context) =>
      contexts.update(uid, context)
      context.addReactor(updatePart(uid))
      context
    case Get(uid) =>
      contexts(uid)
    case Remove(uid) => ???
  }

  def subscriberFor(uid: Uid): Int => Executable = { input =>
    Executable{
      println(s"[RENDER] mapCtxChildren $uid: $input")
    }
  }

  val subscriber: MapCtxModel[Uid, Int] => Executable = { actualMapOfCtx =>
    val executables = actualMapOfCtx.map{case (uid, ctx) =>
      if(!subscribers.contains(uid)) {
        val subscriber = subscriberFor(uid)
        subscribers.update(uid, subscriber)
        Executable {
          ctx.subscribe(subscriber)
          println(s"[RENDER] mapCtx: subscribing to $uid")
        }
      } else Executable {}
    }
    Executable {
      executables.foreach(_.run())
    }
  }

  mapCtx.subscribe(subscriber)

  ctx ! AddParticipant(Uid(0))
//  println(ctx.lastItem)
//  println(mapCtx.lastItem)

  ctx ! IncParticipant(Uid(0))
//  println(ctx.lastItem)
//  println(mapCtx.lastItem)

  ctx ! IncParticipant(Uid(0))
//  println(ctx.lastItem)
//  println(mapCtx.lastItem)

  ctx ! AddParticipant(Uid(1))
//  println(ctx.lastItem)
//  println(mapCtx.lastItem)

  ctx ! IncParticipant(Uid(0))
//  println(ctx.lastItem)
//  println(mapCtx.lastItem)

  ctx ! IncParticipant(Uid(1))
//  println(ctx.lastItem)
//  println(mapCtx.lastItem)

  /**
   * ActiveCtx
   */
  object ActiveCtx {
    def apply[A](initialState: A, compScheduler: Scheduler, ioScheduler: Scheduler): Ctx[A] = new Ctx[A] {
      val lastItem = new AtomicReference[A](initialState)

      override val chain: UpdateChain[A] = UpdateChain()
      override val render: SideChain[A] = SideChain[A]

      private[this] val s = Subject[Action]
      private[this] val subscribeQueue = new ConcurrentLinkedQueue[Executable]()

      s.observeOn(compScheduler).map { action =>
        println(s"======================== Action: $action =================")
        val oldState = lastItem.get
        val newState = chain.update(action, oldState)
        lastItem.set(newState)
        (oldState, newState)
      }.observeOn(compScheduler).map { case (oldState, newState) =>
        val res = render.update(newState)

        Executable {
          while (!subscribeQueue.isEmpty) {
            subscribeQueue.poll().run()
          }
          res.run()
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

      override def subscribe(subscriber:  A => Executable): Unit = {
        render.subscribe(subscriber)
        val actual = lastItem.get
        subscribeQueue offer Executable {
          if (actual != null) {
            subscriber(actual).run()
          }
        }
      }

    }
  }

}


