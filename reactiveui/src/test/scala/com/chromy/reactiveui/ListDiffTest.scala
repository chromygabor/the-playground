package com.chromy.reactiveui

import org.scalatest.FunSpecLike

import scala.collection.immutable.ListMap

/**
 * Created by chrogab on 2015.07.21..
 */

class ListDiffTest extends FunSpecLike {

  describe("ListDiff.diff") {
    ignore("should be able to add element at the end of the seq") {
      val source = List(10, 11)
      val dest = List(10, 11, 20)
      /**
       * Steps:
       * - List(10, 11, 20)
       */
      val (toAdd, toRemove) = ListDiff.diff(source, dest) {_.toString}
      assert(toAdd == Map("20" ->(20, 2)))
      assert(toRemove == Map())
    }

    ignore("should be able to add two element at the end of the seq") {
      val source = List(10, 11)
      val dest = List(10, 11, 20, 30)
      /**
       * Steps:
       * - List(10, 11, 20)
       * - List(10, 11, 20, 30)
       */

      val (toAdd, toRemove) = ListDiff.diff(source, dest) {_.toString}
      assert(toAdd == Map("20" ->(20, 2),
        "30" ->(30, 3)))

    }

    it("should be able to add two element: one at the begin one at the end of the seq") {
      val source = List(10, 11, 12, 13, 14, 15, 16, 17)
      val dest = List(9, 10, 11, 12, 14, 15, 16, 20)

      val (toRemove, toAdd) = ListDiff.diff(source, dest) {_.toString}

      assert(toRemove == Map(10 -> 0,
        11 -> 1,
        12 -> 2,
        13 -> 3,
        17 -> 7
      ))

      assert(toAdd == Map(9 -> 0,
        10 -> 1,
        11 -> 2,
        12 -> 3,
        20 -> 7
      ))

    }
  }

}