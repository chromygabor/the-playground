package binarytree

import scala.util.matching.Regex

/**
 * Created by cry on 2015.03.06..
 */
sealed trait BST {
  def isEmpty: Boolean
}

case object Empty extends BST {
  def isEmpty = true
}

case class Node(value: Int, left: BST = Empty, right: BST = Empty) extends BST {
  def isEmpty = false
}

object RecursiveBST {

  //"40:,25:L,10:LL,32:LR,78:R"

  def deserializeAlternative(in: String): BST = {
    val myRegexp = "(\\d+):(.*)".r

    def loop(inPath: String, inRest: List[String]): (BST, List[String]) = {

      if(inRest.isEmpty) (Empty, Nil)
      else {
        val head1 = inRest.head
        println(head1)
        val tail = inRest.tail
        val myRegexp(value, path) = head1

        if(inRest.tail.isEmpty) {
          println("new Node: " + value.toInt)
          (Node(value.toInt), tail)
        } else {
          val head2 = inRest.tail.head
          val myRegexp(_, nextPath) = head2

          if(nextPath.length > inPath.length) {
            val (left, tail2) = loop(path, tail)
            val (right, tail3) = loop(path, tail2)
            println("new Node: " + value.toInt +"," + left + "," + right)
            (Node(value.toInt, left, right), tail3)
          } else {
            println("new Node: " + value.toInt)
            (Node(value.toInt), tail)
          }
        }
      }
    }

    val l = in.split(",").toList
    val (root, l2) = loop("", l)
    ???
  }

  implicit class BSTUtils(bst: BST) {

    /**
     * Insert a new element into the tree and return with that new tree
     * @param insertValue
     * @return
     */
    def insert(insertValue: Int): BST = {
      bst match {
        case Empty => Node(insertValue, Empty, Empty)
        case orig@Node(value, left, right) =>
          if (insertValue < value) Node(value, left.insert(insertValue), right)
          else if (insertValue > value) Node(value, left, right.insert(insertValue))
          else orig
      }
    }

    /**
     * Traverse the tree with inOrderMethod
     * @return
     */
    def inOrderTraverse: List[Int] = {
      bst match {
        case Empty => List()
        case Node(value, left, right) => (left, right) match {
          case (Empty, Empty) => List(value)
          case (Empty, _) => value :: right.inOrderTraverse
          case (_, Empty) => left.inOrderTraverse ::: value :: Nil
          case (_, _) => left.inOrderTraverse ::: value :: right.inOrderTraverse
        }
      }
    }

    /**
     * Traverse the tree with inOrderMethod
     * @return
     */
    def preOrderTraverse: List[Int] = {
      bst match {
        case Empty => List[Int]()
        case Node(value, left, right) => (left, right) match {
          case (Empty, Empty) => List(value)
          case (Empty, _) => value :: right.preOrderTraverse
          case (_, Empty) => value :: left.preOrderTraverse
          case (_, _) => value :: left.preOrderTraverse ::: right.preOrderTraverse
        }
      }
    }

    /**
     * Traverse the tree with inOrderMethod
     * @return
     */
    def postOrderTraverse: List[Int] = {
      bst match {
        case Empty =>
          List[Int]()
        case Node(value, left, right) => (left, right) match {
          case (Empty, Empty) =>
            List(value)
          case (_, Empty) =>
            val r = left.postOrderTraverse ::: value :: Nil
            r
          case (Empty, _) =>
            val r = right.postOrderTraverse ::: value :: Nil
            r
          case (_, _) =>
            val r = left.postOrderTraverse ::: right.postOrderTraverse ::: value :: Nil
            r
        }
      }
    }

    /**
     * Get the maximum of the tree
     * @return
     */
    def max: Int = bst match {
      case Empty => throw new IllegalStateException("Max on Empty")
      case Node(value, left, right) => right match {
        case Empty => value
        case _ => right.max
      }
    }

    /**
     * Get the minimum of the tree
     * @return
     */
    def min: Int = bst match {
      case Empty => throw new IllegalStateException("Min on Empty")
      case Node(value, left, right) => left match {
        case Empty => value
        case _ => left.min
      }
    }

    /**
     * Return the size of the tree
     * @return
     */
    def size: Int = bst match {
      case Empty => 0
      case Node(_, left, right) => left.size + right.size + 1
    }

    /**
     * return the height of the tree
     * @return
     */
    def height: Int = {
      def loop(bst: BST, accu: Int): Int = {
        bst match {
          case Empty => accu
          case Node(_, left, right) if (!left.isEmpty || !right.isEmpty) => Math.max(loop(left, accu + 1), loop(right, accu + 1))
          case Node(_, left, right) if (left.isEmpty && right.isEmpty) => accu
        }
      }
      loop(bst, 0)
    }

    def serializeAlternative = {
      import scala.collection.immutable.Queue
      def loop(bst: BST, path: Queue[Char]): List[(Int, Queue[Char])] = {
        bst match {
          case Empty =>      List()
          case Node(value, left, right) => (left, right) match {
            case (Empty, Empty) => List((value, path))
            case (_, Empty) => (value, path) :: loop(left, path :+ 'L')
            case (Empty, _) => (value, path) :: loop(right, path :+ 'R')
            case (_, _) => (value, path) :: loop(left, path :+ 'L') ::: loop(right, path :+ 'R')
          }
        }
      }
      val r = loop(bst, Queue())
      r.map { case (value, path) => s"$value:${path.mkString("", "", "")}" }.mkString("",",","")
    }
  }

}



