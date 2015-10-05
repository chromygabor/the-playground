package com.chromy.reactiveui

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.TestUtil._
import com.chromy.reactiveui.core.misc.{Executable, SideChain}
import com.chromy.reactiveui.core.{Action, UpdateChain}
import org.scalatest.FunSpecLike
import rx.lang.scala.schedulers.{ComputationScheduler, IOScheduler}
import rx.lang.scala.{Scheduler, Subject, Subscriber}
import CtxTest._

/**
 * Created by cry on 2015.07.12..
 */

object CtxTest {
  trait Ctx[A] {
    def !(action: Action) = onAction(action)

    protected def onAction(action: Action): Unit

    def subscribe(subscriber: A => Executable): Unit

    def addReactor(f: (Action, A, A) => A): Unit

    val chain: UpdateChain[A] = UpdateChain[A]

    val render: SideChain[A] = SideChain[A]
  }

  object ActiveCtx {
    def apply[A](initialState: A, compScheduler: Scheduler, ioScheduler: Scheduler): Ctx[A] = new Ctx[A] {
      private val lastState = new AtomicReference[A](initialState)

      private[this] val s = Subject[Action]
      private[this] val r = Subject[Executable]
      private[this] val subscribeQueue = new ConcurrentLinkedQueue[Executable]()

      s.observeOn(compScheduler).map { action =>
        val newState = chain.update(action, lastState.get)
        lastState.set(newState)
        newState
      }.observeOn(compScheduler).foreach { newState =>
        val executables = subscribeQueue.
        val sideEffect = render.update(newState)
        r.onNext(sideEffect)
      }

      r.observeOn(ioScheduler).subscribe(new Subscriber[Executable]() {
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
        r onNext Executable {
          val actual = lastState.get
          if (actual != null) {
            subscriber(actual).run()
          }
        }
      }

      override def addReactor(f: (Action, A, A) => A): Unit = chain.subscribe(f)
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


  describe("Context") {
    it("should be able to be created and store actual state and on subscribe it should be promoted") {

      val atomicState = new AtomicReference[SimpleModel]()

      val ctx = ActiveCtx(SimpleModel(), ComputationScheduler(), IOScheduler())

      ctx.addReactor(updateSimple)

      ctx ! AddNumber(10)
      ctx ! AddNumber(30)
      Thread.sleep(100)

      assert(atomicState.get == null)

      ctx.subscribe { input =>
        Executable {
          atomicState.set(input)
        }
      }
      //subscribeExecutable.run()
      Thread.sleep(100)

      assert(atomicState.get == SimpleModel(SubSubModel(40)))
      Thread.sleep(100)

      ctx ! AddNumber(30)
      Thread.sleep(100)
      assert(atomicState.get ==SimpleModel(SubSubModel(70)))
    }
  }
}
