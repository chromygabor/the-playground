package com.chromy.reactiveui

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.CtxTest._
import com.chromy.reactiveui.TestUtils._
import com.chromy.reactiveui.core.misc.{Executable, SideChain}
import com.chromy.reactiveui.core.{Action, UpdateChain}
import rx.lang.scala.schedulers.ImmediateScheduler
import rx.lang.scala.{Scheduler, Subject, Subscriber}


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

object CtxTest {

  trait Ctx[A] {

    def !(action: Action) = onAction(action)

    protected def onAction(action: Action): Unit

    def subscribe(subscriber: ((Option[A], A)) => Executable): Unit

    def addReactor(f: (Action, A, A) => A): Unit = chain.subscribe(f)

    val chain: UpdateChain[A]

    val render: SideChain[(Option[A], A)]

    val lastItem: AtomicReference[A]

//    def map[B](lens: Lens[A, B]): Ctx[B] = {
//      val parent = this
//      new Ctx[B] {
//        override protected def onAction(action: Action): Unit = parent.onAction(action)
//
//        override def subscribe(subscriber: (B) => Executable): Unit = parent.subscribe(a => subscriber(lens.get(a)))
//
//        override val chain: UpdateChain[B] = parent.chain.map(lens)
//        override val render: SideChain[B] = parent.render.map(lens.get)
//        override val lastItem: AtomicReference[B] = new AtomicReference[B](lens.get(parent.lastItem.get()))
//      }
//    }

  }

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
          while(!subscribeQueue.isEmpty) {
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


object CtxTest2 extends App {
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

  val innerUpdate: (Action, MapModel, MapModel) => MapModel = { (action, original, model) =>
    println(s"[UPDATE] innerUpdate: $action, $model")
    model
  }

  def getUpdatesFor(left: Option[MapModel], right: MapModel): List[Uid] = {
    val original = left.getOrElse(MapModel())
    if(original != right ) {
      right.values.foldLeft(List[Uid]()) { case (accu, (partId, value)) =>
        original.values.contains(partId) match {
          case true => accu
          case false => partId :: accu
        }
      }
    } else List()
  }


  val ctx = ActiveCtx(MapModel(), ImmediateScheduler(), ImmediateScheduler())
  ctx.chain.subscribe(updateMap)

  ctx.subscribe { case(oldModel, model) =>
    val updates = getUpdatesFor(oldModel, model)

    val ctxs = updates.map { uid =>
      val newCtx = new Ctx[Int] {

        override protected def onAction(action: Action): Unit = ctx ! action

        override def subscribe(subscriber: ((Option[Int], Int)) => Executable): Unit = {
          ctx.render.subscribe {  case(oldModel, model) =>
            subscriber(oldModel.flatMap(_.values.get(uid)), model.values(uid))
          }
        }

        override val chain: UpdateChain[Int] = UpdateChain()
        override lazy val lastItem: AtomicReference[Int] = new AtomicReference[Int](ctx.lastItem.get().values(uid))
        override val render: SideChain[(Option[Int], Int)] = SideChain()

        private[this] val update: (Action, MapModel, MapModel) => MapModel = { (action, original, model) =>
          model.copy(values = model.values.updated(uid, chain.update(action, model.values(uid))))
        }
        ctx.addReactor(update)
        addReactor(updatePart(uid))
      }

      newCtx.subscribe { case (oldModel, model) =>
          Executable {
            println(s"[RENDER] newCtx($uid): $oldModel -> $model")
          }
      }
      newCtx
    }

    Executable {
      println(s"[RENDER] ctx: $oldModel -> $model, $ctxs")
    }
  }

  ctx ! AddParticipant(0)
  ctx ! IncParticipant(0)
  ctx ! IncParticipant(0)

  ctx ! AddParticipant(1)

  ctx ! IncParticipant(0)
  ctx ! IncParticipant(0)
  ctx ! IncParticipant(0)

  //println(ctx.lastItem)

}
