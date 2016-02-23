package algorythms

import org.scalatest.FunSpecLike

import scala.annotation.tailrec

/**
 * Created by chrogab on 2016.01.28..
 */
object BubbleSort {
  def imperative(unsorted: Array[Int]): Array[Int] = {
    for (y <- 1 until unsorted.length) {
      for (x <- 0 until unsorted.length - y) {
        if (unsorted(x) > unsorted(x + 1)) {
          val tmp = unsorted(x)
          unsorted(x) = unsorted(x + 1)
          unsorted(x + 1) = tmp
        }
      }
    }
    unsorted
  }

  def functional1(input: List[Int]): List[Int] = {

    def loop(unsorted: List[Int], result: List[Int] = Nil): List[Int] = {
      if (unsorted.isEmpty) result
      else innerLoop(unsorted, Nil, result)
    }

    def innerLoop(unsorted: List[Int], rest: List[Int] = Nil, result: List[Int]): List[Int] = {
      unsorted match {
        case h1 :: h2 :: tail if h1 > h2 =>
          innerLoop(h1 :: tail, h2 :: rest, result)
        case h1 :: h2 :: tail =>
          innerLoop(h2 :: tail, h1 :: rest, result)
        case h1 :: Nil =>
          loop(rest, h1 :: result)
      }
    }

    loop(input)
  }

  def functionalFinal(unsorted: Array[Int]): Array[Int] = {
    @tailrec
    def innerLoop(r: List[Int], rest: List[Int] = Nil, result: List[Int]): List[Int] = {
      if (r.isEmpty) result
      else {
        r match {
          case h1 :: h2 :: tail if h1 > h2 => innerLoop(h1 :: tail, h2 :: rest, result)
          case h1 :: h2 :: tail => innerLoop(h2 :: tail, h1 :: rest, result)
          case h1 :: Nil => innerLoop(rest, Nil, h1 :: result)
        }
      }
    }

    innerLoop(unsorted.toList, Nil, Nil).toArray
  }

  def bubbleSort[A: Ordering](unsorted: Seq[A])(implicit ev: Ordering[A]): Seq[A] = {
    @tailrec
    def innerLoop(r: List[A], rest: List[A] = Nil, result: List[A]): List[A] = {
      r match {
        case h1 :: h2 :: tail if ev.compare(h1, h2) > 0 => innerLoop(h1 :: tail, h2 :: rest, result)
        case h1 :: h2 :: tail => innerLoop(h2 :: tail, h1 :: rest, result)
        case h1 :: Nil => innerLoop(rest, Nil, h1 :: result)
        case Nil => result
      }
    }

    innerLoop(unsorted.toList, Nil, Nil).toSeq
  }
}

class BubbleSortTest extends FunSpecLike {

  describe("bubbleSort") {
    it("imperative should sort a list of numbers") {
      val unsorted = Array(2, 6, 25, 8, 3, 20, 1)
      val sorted = BubbleSort.imperative(unsorted).toList

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
