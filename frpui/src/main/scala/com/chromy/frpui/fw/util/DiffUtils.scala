package com.chromy.frpui.fw.util

import com.chromy.frpui.fw.util.DiffUtils._
import org.scalatest.FunSpecLike

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Created by cry on 2015.10.17..
 */

object DiffUtils {

  sealed trait ListOp[+T]

  case class Insert[T](index: Int, item: T) extends ListOp[T]

  case class Remove(index: Int) extends ListOp[Nothing]

  case class Move(from: Int, to: Int) extends ListOp[Nothing]

  sealed trait MapOp[K, V]

  case class KeyUpdate[K, V](key: K, value: V) extends MapOp[K, V]

  case class KeyRemove[K, V](key: K) extends MapOp[K, V]

  sealed trait SetOp[T]

  case class ItemAdd[T](item: T) extends SetOp[T]

  case class ItemRemove[T](item: T) extends SetOp[T]

  implicit def listToDiff[T](left: Seq[T]): ListUtils[T] = new ListUtils(left)

  implicit def mapToDiff[K, V](left: Map[K, V]): MapUtils[K, V] = new MapUtils(left)

  implicit def setToDiff[T](left: Set[T]): SetUtils[T] = new SetUtils(left)
}


class MapUtils[K, V](left: Map[K, V]) {

  def diffOps(right: Map[K, V], trackValueChanges: Boolean = false): Seq[MapOp[_ <: K, _ <: V]] = {

    val l1 = left.foldLeft(Map[K, (Option[V], Option[V])]()) { case (accu, (key, _)) =>
      accu.updated(key, (left.get(key), None))
    }

    val l2 = right.foldLeft(l1) { case (accu, (key, _)) =>
      val nv = accu.get(key).map { in => (in._1, right.get(key)) }
      accu.updated(key, nv.getOrElse(None, right.get(key)))
    }

    val res = if(!trackValueChanges) l2.filter { case (_, (oov, onv)) => !oov.isDefined || !onv.isDefined} else l2.filter { case (_, (oov, onv)) => oov != onv }

    res.map { case (key, (oov, onv)) =>
      (oov, onv) match {
        case (None, Some(nv)) => KeyUpdate(key, nv)
        case (Some(_), Some(nv)) => KeyUpdate(key, nv)
        case (Some(_), None) => KeyRemove(key)
        case _ => throw new IllegalStateException("Both values are None")
      }
    } toSeq : Seq[MapOp[_ <: K, _ <: V]]
  }
}

class SetUtils[T](left: Set[T]) {
  def diffOps(right: Set[T]): Seq[SetOp[_ <: T]] = {
    val (l, r) = left.foldLeft((left, right)) { case ((accuLeft, accuRight), item) =>
      if (right(item)) {
        (accuLeft.filter(_ != item), accuRight.filter(_ != item))
      } else {
        (accuLeft, accuRight)
      }
    }

    l.map(item => ItemRemove(item)) ++ r.map(item => ItemAdd(item)) toSeq
  }
}

class ListUtils[T](left: Seq[T]) {

  def diffOps[Key](right: Seq[T])(createUid: T => Key): Seq[ListOp[_ <: T]] = {
    {
      val mn = right.zipWithIndex.map { case (item, index) =>
        createUid(item) ->(item, index)
      }.toMap

      val s0 = left.zipWithIndex.foldLeft(Map.empty[Key, (Option[Int], Option[Int])]) { case (accu, (item, index)) =>
        accu.updated(createUid(item), (Some(index), Option.empty[Int]))
      }

      val s1 = right.zipWithIndex.foldLeft(s0) { case (accu, (item, index)) =>
        val key = createUid(item)
        val oldValue = accu.get(key).flatMap(_._1)
        accu.updated(key, (oldValue, Some(index)))
      }

      val s3 = s1.toList.sortBy {
        case (_, (_, Some(np))) => np
        case _ => -1
      }

      @tailrec
      def loop1(tail: List[(Key, (Option[Int], Option[Int]))], res: List[(Key, (Option[Int], Option[Int]))], ops: List[ListOp[_ <: T]]): (List[(Key, (Option[Int], Option[Int]))], List[ListOp[_ <: T]]) = {
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
      def loop2(tail: List[(Key, (Option[Int], Option[Int]))], res: List[(Key, (Option[Int], Option[Int]))], ops: List[ListOp[_ <: T]]): (List[(Key, (Option[Int], Option[Int]))], List[ListOp[_ <: T]]) = {
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
      def delete(list: List[(Key, (Option[Int], Option[Int]))], iKey: Key, iOp: Option[Int], iNp: Option[Int]): List[(Key, (Option[Int], Option[Int]))] = {
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


      def insert(list: List[(Key, (Option[Int], Option[Int]))], iKey: Key, iOp: Option[Int], iNp: Option[Int]): List[(Key, (Option[Int], Option[Int]))] = {
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
  }
}

class SetDiffTest extends FunSpecLike {

  import DiffUtils._

  describe("SetDiff should produce operations by two immutable set") {
    it("where operations should be able to build the same mutable set as the immutable set on the right side") {
      case class Item(value: Int)

      val left = Set(Item(10), Item(20), Item(30), Item(40), Item(55))
      val right = Set(Item(5), Item(30), Item(10), Item(35), Item(40), Item(50))

      val testSet = scala.collection.mutable.Set[Item](left.toList: _*)

      left.diffOps(right).foreach {
        case ItemAdd(item) => testSet.add(item)
        case ItemRemove(item) => testSet.remove(item)
      }

      assert(Set(testSet.toList: _*) == right)
    }
  }
}


class MapDiffTest extends FunSpecLike {

  import DiffUtils._

  describe("MapDiff should produce operations by two immutable map") {
    it("where operations should be able to build the same mutable map as the immutable map on the right side") {
      case class Item(value: Int)

      val left = Map(0 -> Item(10), 1 -> Item(20), 2 -> Item(30), 3 -> Item(40), 4 -> Item(55))
      val right = Map(0 -> Item(5), 2 -> Item(30), 5 -> Item(10), 6 -> Item(35), 8 -> Item(40), 10 -> Item(50))

      val testMap = scala.collection.mutable.Map[Int, Item](left.toList: _*)

      left.diffOps(right, true) foreach {
        case KeyUpdate(key, value) => testMap.update(key, value)
        case KeyRemove(key) => testMap.remove(key)
      }

      assert(Map(testMap.toList: _*) == right)


      val left2 = Map(0 -> Item(10), 1 -> Item(20), 2 -> Item(30), 3 -> Item(40), 4 -> Item(55))
      val right2 = Map(0 -> Item(10), 1 -> Item(20), 2 -> Item(30), 3 -> Item(40), 4 -> Item(55))

      assert(left2.diffOps(right2).mkString("\n").size == 0)

      val left3 = Map(0 -> Item(10), 1 -> Item(20), 2 -> Item(30), 3 -> Item(40), 4 -> Item(55))
      val right3 = Map(0 -> Item(11), 1 -> Item(21), 2 -> Item(31), 3 -> Item(41), 4 -> Item(56))

      assert(left3.diffOps(right3).mkString("\n").size == 0)
    }
  }
}

class ListDiffTest extends FunSpecLike {

  import DiffUtils._

  describe("ListDiff should produce operations by two immutable list") {

    it("where operations should be able to build the same mutable list as the immutable list on the right side") {
      case class Item(value: Int)

      val left = List(Item(10), Item(20), Item(30), Item(40), Item(55))
      val right = List(Item(5), Item(30), Item(10), Item(35), Item(40), Item(50))

      val testArray = ArrayBuffer(left: _*)
      left.diffOps(right) { case Item(key) => key }.foreach {
        case Insert(pos, item) =>
          testArray.insert(pos, item)
        case Remove(pos) =>
          testArray.remove(pos)
        case Move(from, to) =>
          val item = testArray.remove(from)
          testArray.insert(to, item)
      }

      assert(testArray.toList == right)
    }
  }
}
