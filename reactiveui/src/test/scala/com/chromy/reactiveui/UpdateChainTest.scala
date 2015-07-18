package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx.{Action, LocalAction}
import monocle.macros.GenLens
import org.scalatest.FunSpecLike

/**
 * Created by cry on 2015.05.30..
 */

class UpdateChainTest extends FunSpecLike {

  case class AddNumber(number: Int) extends Action

  case class MulNumber(number: Int) extends LocalAction

  case class MainModel(leftSub: SubModel = SubModel(), rightSub: SubModel = SubModel())

  case class SubModel(leftSubSub: SubSubModel = SubSubModel(), rightSubSub: SubSubModel = SubSubModel())

  case class SubSubModel(value: Int = 0)


  describe("dispatcher") {
    it("should have fromLens method") {
      def updateSub: (Action, SubModel) => SubModel = { (action, model) =>
        model
      }

      def updateSubSub: (Action, SubSubModel) => SubSubModel = { (action, model) =>
        action match {
          case AddNumber(toAdd) => model.copy(value = model.value + toAdd)
          case MulNumber(toMul) => model.copy(value = model.value * toMul)
        }
      }

      val root = UpdateChain[MainModel]()

      val leftSubMapper = root.fromLens(GenLens[MainModel](_.leftSub))
      val rightSubMapper = root.fromLens(GenLens[MainModel](_.rightSub))

      leftSubMapper.subscribe(updateSub)
      rightSubMapper.subscribe(updateSub)

      val leftLeftSubSubMapper = leftSubMapper.fromLens(GenLens[SubModel](_.leftSubSub))
      val leftRightSubSubMapper = leftSubMapper.fromLens(GenLens[SubModel](_.rightSubSub))
      val rightLeftSubSubMapper = rightSubMapper.fromLens(GenLens[SubModel](_.leftSubSub))
      val rightRightSubSubMapper = rightSubMapper.fromLens(GenLens[SubModel](_.rightSubSub))

      leftLeftSubSubMapper.subscribe(updateSubSub)
      leftRightSubSubMapper.subscribe(updateSubSub)
      rightLeftSubSubMapper.subscribe(updateSubSub)
      rightRightSubSubMapper.subscribe(updateSubSub)

      val result1 = root.update(AddNumber(10), MainModel())
      assert(result1.leftSub.leftSubSub.value == 10)
      assert(result1.leftSub.rightSubSub.value == 10)

      assert(result1.rightSub.leftSubSub.value == 10)
      assert(result1.rightSub.rightSubSub.value == 10)

      val result2 = root.update(AddNumber(20), result1)
      assert(result2.leftSub.leftSubSub.value == 30)
      assert(result2.leftSub.rightSubSub.value == 30)

      assert(result2.rightSub.leftSubSub.value == 30)
      assert(result2.rightSub.rightSubSub.value == 30)

    }
  }
}