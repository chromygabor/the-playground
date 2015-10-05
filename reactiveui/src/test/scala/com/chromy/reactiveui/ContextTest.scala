package com.chromy.reactiveui

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.TestUtil._
import com.chromy.reactiveui.core.misc.{Executable, SideChain}
import com.chromy.reactiveui.core.{Action, UpdateChain}
import monocle.Lens
import monocle.macros.GenLens
import org.scalatest.FunSpecLike
import rx.lang.scala.schedulers.{ComputationScheduler, IOScheduler}
import rx.lang.scala.{Scheduler, Subject, Subscriber}
import CtxTest._
import scala.collection.JavaConversions._

/**
 * Created by cry on 2015.07.12..
 */

object TestUtil {
  case class SubSubModel(value: Int = 0)
  case class SimpleModel(sub: SubSubModel = SubSubModel())

  case class AddNumber(number: Int) extends Action
}

object CtxTest {
  trait Ctx[A] {

    def !(action: Action) = onAction(action)

    protected def onAction(action: Action): Unit

    def subscribe(subscriber: A => Executable): Unit

    def addReactor(f: (Action, A, A) => A): Unit = chain.subscribe(f)

    val chain: UpdateChain[A]

    val render: SideChain[A]

    def map[B](lens: Lens[A,B]): Ctx[B] = {
      val parent = this
      new Ctx[B] {
        override protected def onAction(action: Action): Unit = parent.onAction(action)

        override def subscribe(subscriber: (B) => Executable): Unit = parent.subscribe (a => subscriber(lens.get(a)))

        override val chain: UpdateChain[B] = parent.chain.map(lens)
        override val render: SideChain[B] = parent.render.map(lens.get)
      }
    }

  }

  object ActiveCtx {
    def apply[A](initialState: A, compScheduler: Scheduler, ioScheduler: Scheduler): Ctx[A] = new Ctx[A] {
      private val lastState = new AtomicReference[A](initialState)

      override val chain: UpdateChain[A] = UpdateChain()
      override val render: SideChain[A] = SideChain[A]

      private[this] val s = Subject[Action]
      private[this] val subscribeQueue = new ConcurrentLinkedQueue[Executable]()

      s.observeOn(compScheduler).map { action =>
        val newState = chain.update(action, lastState.get)
        lastState.set(newState)
        newState
      }.observeOn(compScheduler).map { newState =>
        val res = render.update(newState)

        Executable {
          subscribeQueue.iterator.toList.foreach(_.run())
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

      override def subscribe(subscriber: (A) => Executable): Unit = {
        render.subscribe(subscriber)
        val actual = lastState.get
        subscribeQueue offer Executable {
          if (actual != null) {
            subscriber(actual).run()
          }
        }
      }

    }
  }

}



class ContextTest extends FunSpecLike {

  val updateSimple: (Action, SimpleModel, SimpleModel) => SimpleModel = { (action, _, model) =>
    action match {
      case AddNumber(number) => model.copy(sub = model.sub.copy(value = model.sub.value + number))
      case _ => model
    }
  }

  val updateSubSub: (Action, SubSubModel, SubSubModel) => SubSubModel = { (action, _, model) =>
    action match {
      case AddNumber(number) => model.copy(value = model.value + number)
      case _ => model
    }
  }


  describe("Context") {
    ignore("should be able to be created and store actual state and on subscribe it should be promoted") {

      val atomicState = new AtomicReference[SimpleModel]()

      val ctx = ActiveCtx(SimpleModel(), ComputationScheduler(), IOScheduler())

      ctx.addReactor(updateSimple)

      ctx ! AddNumber(10)
      ctx ! AddNumber(30)
      Thread.sleep(100)

      assert(atomicState.get == null)

      ctx.subscribe { input =>
        Executable {
          println(s"SideEffect: $input")
          atomicState.set(input)
        }
      }

      ctx ! AddNumber(30)
    }

    it("should be able be mapped through a lens") {
      val ctx = ActiveCtx(SimpleModel(), ComputationScheduler(), IOScheduler())
      val newContext = ctx.map(GenLens[SimpleModel](_.sub))

      newContext.addReactor(updateSubSub)

      ctx ! AddNumber(10)
      ctx ! AddNumber(30)
      Thread.sleep(100)

      ctx.subscribe { input =>
        Executable {
          println(s"SideEffect: $input")
        }
      }

      ctx ! AddNumber(30)

    }
  }
}
