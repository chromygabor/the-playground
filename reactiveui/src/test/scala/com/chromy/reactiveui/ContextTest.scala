package com.chromy.reactiveui

import java.util.concurrent.{Executor, Executors}

import com.chromy.reactiveui.TestUtil.{AddNumber, SimpleModel, SubSubModel}
import com.chromy.reactiveui.core.{Action, LocalAction, Context, UpdateChain}
import monocle.macros.GenLens
import org.scalatest.FunSpecLike
import rx.lang.scala.{Observable, Observer, Subject, Subscriber}

/**
 * Created by cry on 2015.07.12..
 */

object TestUtil {
  case class SubSubModel(value: Int = 0)

  case class SimpleModel(sub: SubSubModel = SubSubModel())

  case class AddNumber(number: Int) extends Action

  case class MulNumber(number: Int) extends LocalAction

  val updateSubSub: (Action, SubSubModel, SubSubModel) => SubSubModel = { (action, _, model) =>
    action match {
      case AddNumber(toAdd) => model.copy(value = model.value + toAdd)
      case MulNumber(toMul) => model.copy(value = model.value * toMul)
    }
  }

  val updateSimple: (Action, SimpleModel, SimpleModel) => SimpleModel = { (action, _, model) =>
    model
  }


}

class ContextTest extends FunSpecLike {
  describe("Context") {
    type A = SimpleModel
    var action: Action = null

    val simpleContext = new Context[A] {
      override val changes: Observable[A] = Subject[A]

      override val chain: UpdateChain[A] = UpdateChain[A]

      override val changesExecutor: Executor = Executors.newSingleThreadExecutor()

      override val chainExecutor: Executor = Executors.newSingleThreadExecutor()

      override val channel = Subject[Action]
      channel.subscribe(new Subscriber[Action]() {
        override def onNext(value: Action): Unit = action = value
      })
    }

    val subMapper = simpleContext.map(GenLens[SimpleModel](_.sub))
    val subContext = subMapper(TestUtil.updateSubSub)

    it("channel should propagate up the actions 20000x") {

      val result = List.range(0, 20000).foreach{ i =>
        subContext.channel.onNext(AddNumber(i))
      }

      assert(action == AddNumber(19999))
    }
  }
}