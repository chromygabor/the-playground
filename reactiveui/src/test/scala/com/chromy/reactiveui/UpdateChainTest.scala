package com.chromy.reactiveui

import java.util.UUID

import com.chromy.reactiveui.myjavafx.Action
import monocle.macros.GenLens
import org.scalatest.FunSpecLike

/**
 * Created by cry on 2015.05.30..
 */

case class MainModel(left: SubModel = SubModel(), right: SubModel = SubModel(), uid: String = UUID.randomUUID().toString)

case class SubModel(value: Int = 0)

case class Add(number: Int) extends Action

case class Mul(number: Int) extends Action

class UpdateChainTest extends FunSpecLike {
  def update: (Action, SubModel) => SubModel = { (action, model) =>
    action match {
      case Add(toAdd) => model.copy(value = model.value + toAdd)
      case Mul(toMul) => model.copy(value = model.value * toMul)
    }
  }

  describe("dispatcher") {
    it("should have fromLens method") {
      val root = UpdateChain[MainModel]()


      val leftFactory = root.fromLens(GenLens[MainModel](_.left))
      val rightFactory = root.fromLens(GenLens[MainModel](_.right))

      val left1 = leftFactory.subscribe(update)
      val right2 = rightFactory.subscribe(update)

      val result1 = root.update(Add(10), MainModel())
      assert(result1 == MainModel(left = SubModel(10), right = SubModel(10)))

      val result2 = root.update(Add(20), result1)
      assert(result2 == MainModel(left = SubModel(30), right = SubModel(30)))

    }
  }


}