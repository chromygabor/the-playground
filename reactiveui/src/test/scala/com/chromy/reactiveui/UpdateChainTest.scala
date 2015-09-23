//package com.chromy.reactiveui
//
//import com.chromy.reactiveui.TestUtil.{AddNumber, SimpleModel, SubSubModel}
//import com.chromy.reactiveui.core.{Action, LocalAction, UpdateChain}
//import monocle.macros.GenLens
//import org.scalatest.FunSpecLike
//
///**
// * Created by cry on 2015.05.30..
// */
//
//
//class UpdateChainTest extends FunSpecLike {
//
//
//  case class ComplexModel(leftSub: SubModel = SubModel(), rightSub: SubModel = SubModel())
//
//  case class SubModel(leftSubSub: SubSubModel = SubSubModel(), rightSubSub: SubSubModel = SubSubModel())
//
//
//  val updateSub: (Action, SubModel, SubModel) => SubModel = { (action, _, model) =>
//    model
//  }
//
//  describe("UpdateChain") {
//    it("should be able to update subscribers through lens") {
//
//      val root = UpdateChain[ComplexModel]()
//
//      val leftSubMapper = root.map(GenLens[ComplexModel](_.leftSub))
//      val rightSubMapper = root.map(GenLens[ComplexModel](_.rightSub))
//
//      leftSubMapper.subscribe(updateSub)
//      rightSubMapper.subscribe(updateSub)
//
//      val leftLeftSubSubMapper = leftSubMapper.map(GenLens[SubModel](_.leftSubSub))
//      val leftRightSubSubMapper = leftSubMapper.map(GenLens[SubModel](_.rightSubSub))
//      val rightLeftSubSubMapper = rightSubMapper.map(GenLens[SubModel](_.leftSubSub))
//      val rightRightSubSubMapper = rightSubMapper.map(GenLens[SubModel](_.rightSubSub))
//
//      leftLeftSubSubMapper.subscribe(TestUtil.updateSubSub)
//      leftRightSubSubMapper.subscribe(TestUtil.updateSubSub)
//      rightLeftSubSubMapper.subscribe(TestUtil.updateSubSub)
//      rightRightSubSubMapper.subscribe(TestUtil.updateSubSub)
//
//
//      val result1 = root.update(AddNumber(10), ComplexModel())
//      assert(result1.leftSub.leftSubSub.value == 10)
//      assert(result1.leftSub.rightSubSub.value == 10)
//
//      assert(result1.rightSub.leftSubSub.value == 10)
//      assert(result1.rightSub.rightSubSub.value == 10)
//
//      val result2 = root.update(AddNumber(20), result1)
//      assert(result2.leftSub.leftSubSub.value == 30)
//      assert(result2.leftSub.rightSubSub.value == 30)
//
//      assert(result2.rightSub.leftSubSub.value == 30)
//      assert(result2.rightSub.rightSubSub.value == 30)
//
//      val result = List.range(0, 20000).foldLeft(ComplexModel()) { (actModel, _) =>
//        root.update(AddNumber(1), actModel)
//      }
//
//      assert(result.leftSub.leftSubSub.value == 20000)
//      assert(result.leftSub.rightSubSub.value == 20000)
//    }
//    it("should be able to update 20000 times") {
//      val root = UpdateChain[SimpleModel]()
//      root.subscribe(TestUtil.updateSimple)
//
//      val sub = root.map(GenLens[SimpleModel](_.sub))
//      sub.subscribe(TestUtil.updateSubSub)
//
//      val result = List.range(0, 20000).foldLeft(SimpleModel()) { (actModel, _) =>
//        root.update(AddNumber(1), actModel)
//      }
//
//      assert(result.sub.value == 20000)
//    }
//  }
//}