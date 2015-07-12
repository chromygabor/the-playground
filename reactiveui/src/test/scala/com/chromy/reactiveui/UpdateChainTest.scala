package com.chromy.reactiveui

import java.util.UUID

import com.chromy.reactiveui.myjavafx.Action
import com.chromy.reactiveui.myjavafx.Counters.Add
import monocle.macros.GenLens
import org.scalatest.FunSpecLike

/**
 * Created by cry on 2015.05.30..
 */

class UpdateChainTest extends FunSpecLike {

  describe("dispatcher") {
    it("should have fromLens method") {
      new BaseTest {
        def update: (Action, SubModel) => SubModel = { (action, model) =>
          action match {
            case Add(toAdd) => model.copy(value = model.value + toAdd)
            case Mul(toMul) => model.copy(value = model.value * toMul)
          }
        }

        val root = UpdateChain[MainModel]()


        val leftFactory = root.fromLens(GenLens[MainModel](_.left))
        val rightFactory = root.fromLens(GenLens[MainModel](_.right))

        val left1 = leftFactory.subscribe(update)
        val right2 = rightFactory.subscribe(update)

        val result1 = root.update(Add(10), MainModel())
        assert(result1.left.value == 10)
        assert(result1.right.value == 10)

        val result2 = root.update(Add(20), result1)
        assert(result2.left.value == 30)
        assert(result2.right.value == 30)
      }
    }
  }

}