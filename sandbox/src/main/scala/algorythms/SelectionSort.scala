package algorythms

import org.scalatest.FunSpecLike

/**
 * Created by cry on 2016.02.23..
 */
object InsertionSort {
  def imperative(unsorted: Array[Int]): Array[Int] = {
    
    ???
  }

}

class InsertionSortTest extends FunSpecLike {
  describe("InsertionSort") {
    it("imperative should sort a list of numbers") {
      val unsorted = Array(2, 6, 25, 8, 3, 20, 1)
      val sorted = InsertionSort.imperative(unsorted).toList

      assert(sorted == List(1, 2, 3, 6, 8, 20, 25))
    }

    it("functional1 should sort a list of numbers") {
      val unsorted = Array(2, 6, 25, 8, 3, 20, 1)
      val sorted = BubbleSort.functional1(unsorted.toList)

      assert(sorted == List(1, 2, 3, 6, 8, 20, 25))
    }

    it("functionalFinal should sort a list of numbers") {
      val unsorted = Array(2, 6, 25, 8, 3, 20, 1)
      val sorted = BubbleSort.functionalFinal(unsorted).toList

      assert(sorted == List(1, 2, 3, 6, 8, 20, 25))
    }

    it("bubbleSort should sort a list of numbers") {
      val unsorted = Array(2, 6, 25, 8, 3, 20, 1)
      val sorted = BubbleSort.bubbleSort(unsorted).toList

      assert(sorted == List(1, 2, 3, 6, 8, 20, 25))
    }
  }
}
