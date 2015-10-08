package com.chromy.reactiveui

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.TestUtils._
import com.chromy.reactiveui.core.misc.{Executable, SideChain}
import com.chromy.reactiveui.core.{Action, UpdateChain}
import monocle.Lens
import monocle.macros.GenLens
import rx.lang.scala.schedulers.ImmediateScheduler
import rx.lang.scala.{Scheduler, Subject, Subscriber}

import scala.collection.mutable.{Map => MMap}


/**
 * Created by cry on 2015.07.12..
 */

object TestUtils {
  type Uid = Int

  case class SubSubModel(value: Int = 0)

  case class SimpleModel(sub: SubSubModel = SubSubModel())


  case class AddNumber(number: Int) extends Action

  case class MapModel(values: Map[Uid, Int] = Map())

  case class AddParticipant(partId: Int) extends Action

  case class IncParticipant(partId: Int) extends Action

}

object CtxTest extends App {
  sealed trait MapOperation[K,V]

  case class Add[K,V](uid: K, context: Ctx[V]) extends MapOperation[K,V]

  case class Remove[K,V](uid: K) extends MapOperation[K,V]

  case class Get[K,V](uid: K) extends MapOperation[K,V]

  trait Ctx[A] {

    def !(action: Action) = onAction(action)

    protected def onAction(action: Action): Unit

    def subscribe(subscriber: ((Option[A], A)) => Executable): Unit

    def addReactor(f: (Action, A, A) => A): Unit = chain.subscribe(f)

    def chain: UpdateChain[A]

    def render: SideChain[(Option[A], A)]

    def lastItem: AtomicReference[A]

    def newBy[B](lens: Lens[A, B]): CtxApplier[A, B] = new CtxApplier[A, B](this, lens)
  }

  class CtxApplier[A, B](parent: Ctx[A], lens: Lens[A, B]) {
    def apply(): Ctx[B] = new LensCtx(parent, lens)

    def toMapCtx[K, V](f: (MapOperation[K, V]) => Ctx[V])(implicit ev: B <:< Map[K, V]): Ctx[Seq[MapOperation[K,V]]] = {
      new MapCtx[Map[K, V], K, V](apply().asInstanceOf[Ctx[Map[K,V]]]) {
        override val getContext: (MapOperation[K, V]) => Ctx[V] = f
      }.asInstanceOf[Ctx[Seq[MapOperation[K,V]]]]
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
    override protected def onAction(action: Action): Unit = parent ! action

    override val chain: UpdateChain[B] = parent.chain.map(lens)
    override val lastItem: AtomicReference[B] = new AtomicReference[B](lens.get(parent.lastItem.get()))

    override def render: SideChain[(Option[B], B)] = ???

    override def subscribe(subscriber: ((Option[B], B)) => Executable): Unit = ???
  }

  /**
   * Context which can handle a map
   * @param parent
   * @tparam B
   * @tparam K
   * @tparam V
   */

  abstract class MapCtx[B <: Map[K, V], K, V](parent: Ctx[B]) extends Ctx[Seq[MapOperation[K,V]]] {
    val getContext: MapOperation[K,V] => Ctx[V]

    override val lastItem: AtomicReference[Seq[MapOperation[K,V]]] = new AtomicReference[Seq[MapOperation[K,V]]]()
    override def chain: UpdateChain[Seq[MapOperation[K,V]]] = throw new IllegalAccessException("This context doesn't have update chain")

    override def render: SideChain[(Option[Seq[MapOperation[K,V]]], Seq[MapOperation[K,V]])] = SideChain[(Option[Seq[MapOperation[K,V]]], Seq[MapOperation[K,V]])]()


    override def subscribe(subscriber: ((Option[Seq[MapOperation[K,V]]], Seq[MapOperation[K,V]])) => Executable): Unit = ???

    override protected def onAction(action: Action): Unit = parent ! action

    private[this] def diff(left: Map[K, V], right: Map[K, V]): List[K] = {
      if (left != right) {
        right.foldLeft(List[K]()) { case (accu, (partId, value)) =>
          left.contains(partId) match {
            case true => accu
            case false => partId :: accu
          }
        }
      } else List()
    }


    private[this] def createContext(uid: K, initialState: V): Ctx[V] = {
      val parent = this
      new Ctx[V] {
        override protected def onAction(action: Action): Unit = parent ! action

        override val chain: UpdateChain[V] = UpdateChain()

        override def lastItem: AtomicReference[V] = new AtomicReference(initialState)

        override def subscribe(subscriber: ((Option[V], V)) => Executable): Unit = ???

        override def render: SideChain[(Option[V], V)] = ???
      }
    }

    private[this] val update: (Action, B, B) => B = { (action, original, model) =>

      diff(original, model) map { uid =>
        uid -> getContext(Add(uid, createContext(uid, model(uid))))
      }

      val newValues = model.map { case (uid, ctx) =>
        uid -> getContext(Get[K,V](uid)).chain.update(action, model(uid))
      }.asInstanceOf[B]

      //lastItem.set(newValues)
      newValues
    }
    parent.chain.subscribe(update)
  }


  val updateMap: (Action, MapModel, MapModel) => MapModel = { (action, original, model) =>
    println(s"[UPDATE] updateMap: $action, $model")
    action match {
      case AddParticipant(uid) => model.copy(values = model.values.updated(uid, 0))
      case _ => model
    }
  }

  def updatePart(partId: Int): (Action, Int, Int) => Int = { (action, original, model) =>
    println(s"[UPDATE] updatePart for $partId: $action, $model")
    action match {
      case IncParticipant(uid) if uid == partId => model + 1
      case _ => model
    }
  }

  val contexts = MMap[Uid, Ctx[Int]]()

  val ctx = ActiveCtx(MapModel(), ImmediateScheduler(), ImmediateScheduler())
  ctx.addReactor(updateMap)

  ctx.subscribe { case (original, model) =>
    Executable {
      println(s" [RENDER] ctx: $model")
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

  mapCtx.subscribe { case (originalListOfCtx, actualListOfCtx) =>
    Executable {
      println(s" [RENDER] mapCtx: $actualListOfCtx")
    }
  }

  ctx ! AddParticipant(0)
  println(ctx.lastItem)
  println(mapCtx.lastItem)

  ctx ! IncParticipant(0)
  println(ctx.lastItem)
  println(mapCtx.lastItem)

  ctx ! IncParticipant(0)
  println(ctx.lastItem)
  println(mapCtx.lastItem)

  ctx ! AddParticipant(1)
  println(ctx.lastItem)
  println(mapCtx.lastItem)

  ctx ! IncParticipant(0)
  println(ctx.lastItem)
  println(mapCtx.lastItem)

  ctx ! IncParticipant(1)
  println(ctx.lastItem)
  println(mapCtx.lastItem)

  /**
   * ActiveCtx
   */
  object ActiveCtx {
    def apply[A](initialState: A, compScheduler: Scheduler, ioScheduler: Scheduler): Ctx[A] = new Ctx[A] {
      val lastItem = new AtomicReference[A](initialState)

      override val chain: UpdateChain[A] = UpdateChain()
      override val render: SideChain[(Option[A], A)] = SideChain[(Option[A], A)]

      private[this] val s = Subject[Action]
      private[this] val subscribeQueue = new ConcurrentLinkedQueue[Executable]()

      s.observeOn(compScheduler).map { action =>
        println(s"======================== Action: $action =================")
        val oldState = lastItem.get
        val newState = chain.update(action, oldState)
        lastItem.set(newState)
        (oldState, newState)
      }.observeOn(compScheduler).map { case (oldState, newState) =>
        val res = render.update(Some(oldState), newState)

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

      override def subscribe(subscriber: ((Option[A], A)) => Executable): Unit = {
        render.subscribe(subscriber)
        val actual = lastItem.get
        subscribeQueue offer Executable {
          if (actual != null) {
            subscriber(None, actual).run()
          }
        }
      }

    }
  }

}


