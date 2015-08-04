package com.chromy.reactiveui

import scala.collection.immutable.ListMap

/**
 * Created by cry on 2015.07.24..
 */
object ListDiff {

  def diff[A](prev: Seq[A], act: Seq[A])(mapper: (A) => String) = {

    val idxPrev = ListMap((for (in <- prev.zipWithIndex) yield (mapper(in._1) ->(in._1, in._2))): _*)
    val idxAct = ListMap((for (in <- act.zipWithIndex) yield (mapper(in._1) ->(in._1, in._2))): _*)

    val (toAdd, toRemove) = idxAct.zipWithIndex.foldLeft(ListMap[String, (A, Int)](), idxPrev) { case ((toAdd, toRemove), ((hash1, (elem1, index1)), index)) =>
      idxPrev.get(hash1) match {
        case Some((_, index2)) if (index1 == index2) =>
          (toAdd, toRemove - hash1)
        case Some((elem2, index2)) if (index1 != index2) =>
          (toAdd + (hash1 ->(elem2, index)), toRemove)
        case None =>
          (toAdd + (hash1 ->(elem1, index)), toRemove - hash1)
      }
    }

    //    val (_, toDelete) = toRemove.foldLeft((0, List[(A, Int)]())) { case ((removedItems, list), (hash, (item, index))) =>
    //      val newItem = item -> (index - removedItems)
    //      (removedItems + 1, newItem :: list)
    //    }

    (toRemove.map { case (hash, (item, index)) => item -> index }, toAdd.map { case (hash, (item, index)) => item -> index })
  }
}
