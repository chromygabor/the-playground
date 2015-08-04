package algorythms

import scala.annotation.tailrec

/**
 * Created by cry on 2015.07.30..
 */
object MergeSort extends App {

  type A = Int

  def mergeSort(list: List[A]): List[A] = {

    @tailrec
    def merge(lefts: List[A], rights: List[A], acc: List[A]): List[A] = {
      (lefts, rights) match {
        case (Nil, _) => rights ::: acc
        case (_, Nil) => lefts ::: acc
        case (lh :: lt, rh :: rt) =>
          if (lh < rh) merge(lt, rights, lh :: acc)
          else merge(lefts, rt, rh :: acc)
      }
    }

    val n = list.length / 2
    if (n == 0) list
    else {
      val (lefts, rights) = list.splitAt(n)
      merge(mergeSort(lefts), mergeSort(rights), Nil).reverse
    }
  }


  println(mergeSort(List(10, 5, 8, 7, 3, 1, 2, 6, 4, 9)))
}
