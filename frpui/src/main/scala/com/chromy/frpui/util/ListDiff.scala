package com.chromy.frpui.util

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Created by cry on 2015.10.17..
 */
object ListDiff extends App {

  trait ListOp[+T]

  case class Insert[T](index: Int, item: T) extends ListOp[T]

  case class Remove(index: Int) extends ListOp[Nothing]

  case class Move(from: Int, to: Int) extends ListOp[Nothing]


  def diffOps[T](ol: List[T], nl: List[T])(createUid: T => Int): List[ListOp[_ <: T]] = {
    val mn = nl.zipWithIndex.map { case (item, index) =>
      createUid(item) ->(item, index)
    }.toMap

    val s0 = ol.zipWithIndex.foldLeft(Map.empty[Int, (Option[Int], Option[Int])]) { case (accu, (item, index)) =>
      accu.updated(createUid(item), (Some(index), Option.empty[Int]))
    }

    val s1 = nl.zipWithIndex.foldLeft(s0) { case (accu, (item, index)) =>
      val key = createUid(item)
      val oldValue = accu.get(key).flatMap(_._1)
      accu.updated(key, (oldValue, Some(index)))
    }

    val s3 = s1.toList.sortBy {
      case (_, (_, Some(np))) => np
      case _ => -1
    }

    @tailrec
    def loop1(tail: List[(Int, (Option[Int], Option[Int]))], res: List[(Int, (Option[Int], Option[Int]))], ops: List[ListOp[_ <: T]]): (List[(Int, (Option[Int], Option[Int]))], List[ListOp[_ <: T]]) = {
      if (tail.nonEmpty) {
        val (key, (op, np)) = tail.head
        (op, np) match {
          case (None, _) =>
            //println(s"Adding: $key to $np")
            val item = mn(key)._1: T
            loop1(insert(tail.tail, key, np, np), insert(res, key, np, np), Insert(np.get, item) :: ops)
          case (_, None) =>
            //println(s"Removing: $key from $op")
            loop1(delete(tail.tail, key, op, op), delete(res, key, op, op), Remove(op.get) :: ops)
          case _ =>
            loop1(tail.tail, res, ops)
        }
      } else {
        (res, ops)
      }
    }

    @tailrec
    def loop2(tail: List[(Int, (Option[Int], Option[Int]))], res: List[(Int, (Option[Int], Option[Int]))], ops: List[ListOp[_ <: T]]): (List[(Int, (Option[Int], Option[Int]))], List[ListOp[_ <: T]]) = {
      if (tail.nonEmpty) {
        val (key, (Some(op), Some(np))) = tail.head
        if (op != np) {
          //println(s"Moving: $key from: $op, to $np")
          val l1 = (key, (Some(np), Some(np))) :: delete(res, key, Some(op), Some(np))
          val newList = insert(l1, key, Some(np), Some(np))
          loop2(newList.tail, newList, Move(op, np) :: ops)
        } else {
          loop2(tail.tail, res, ops)
        }
      } else {
        (res, ops)
      }
    }
    def delete(list: List[(Int, (Option[Int], Option[Int]))], iKey: Int, iOp: Option[Int], iNp: Option[Int]): List[(Int, (Option[Int], Option[Int]))] = {
      list.filter {
        case (key, (op, np)) => key != iKey
      }.map { case (key, (op, np)) =>
        if (key == iKey) {
          key ->(iOp, iNp)
        } else {
          val cOp = for {
            o1 <- op
            o2 <- iOp
          } yield {
              if (o1 >= o2) o1 - 1
              else o1
            }

          key ->(cOp, np)
        }
      }
    }


    def insert(list: List[(Int, (Option[Int], Option[Int]))], iKey: Int, iOp: Option[Int], iNp: Option[Int]): List[(Int, (Option[Int], Option[Int]))] = {
      list.map { case (key, (op, np)) =>
        if (key == iKey) {
          key ->(iOp, iNp)
        } else {
          val cOp = for {
            o1 <- op
            o2 <- iOp
          } yield {
              if (o1 >= o2) o1 + 1
              else o1
            }

          key ->(cOp, np)
        }
      }
    }

    val (s4, ops) = loop1(s3, s3, Nil)
    val (_, ops2) = loop2(s4, s4, ops)

    ops2.reverse
  }


  case class Item(value: Int)

  val ol = List(Item(10), Item(20), Item(30), Item(40), Item(55))
  val nl = List(Item(5), Item(30), Item(10), Item(35), Item(40), Item(50))

  val testArray = ArrayBuffer(ol: _*)
  diffOps(ol, nl) { case Item(key) => key }.foreach {
    case Insert(pos, item) =>
      testArray.insert(pos, item)
    case Remove(pos) =>
      testArray.remove(pos)
    case Move(from, to) =>
      val item = testArray.remove(from)
      testArray.insert(to, item)
  }

  assert(testArray.toList == nl)
}
