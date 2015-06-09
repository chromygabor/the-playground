package com.chromy.reactiveui

import javafx.scene.Parent

import com.chromy.reactiveui.myjavafx.State
import monocle.Lens
import monocle.macros.GenLens
import org.scalatest.FunSpecLike

/**
 * Created by cry on 2015.05.30..
 */

class DispatcherTest extends FunSpecLike {

  case class Model(left: Int, right: Int)

  sealed trait Op

  case class Add(number: Int) extends Op

  case class Mul(number: Int) extends Op


  def doInt: (Op) => State[Int, Op] = { operation => State { in =>
    val r = operation match {
      case Add(number) => in + number
      case Mul(number) => in * number
    }
    r -> operation
  }
  }

  def update: (Int, Op) => Int = { (prev, op) => op match {
    case Add(number) => prev + number
    case Mul(number) => prev * number
  }
  }

  describe("dispatcher") {
    it("should have fromLens method") {
      val root = Dispatcher[Model, Op]


      val leftFactory = root.fromLens(GenLens[Model](_.left))
      val rightFactory = root.fromLens(GenLens[Model](_.right))

      val left1 = leftFactory.subscribe(update)
      val right2 = rightFactory.subscribe(update)

      val res = for {
        _ <- root.update(Add(10))
        _ <- root.update(Add(20))
        _ <- root.update(Mul(2))
      } yield ()

      val result = res.run(Model(10, 10))._1
      assert(result == Model(80, 80))

    }
  }


}