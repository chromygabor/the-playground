package com.msci.playground

/**
 * Created by cry on 2015.01.28..
 */

trait State[S, +A] {
  def run: S => (S, A)

  def map[B](f: A => B): State[S, B] = State { s =>
    val (s1, a) = run(s)
    (s1, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (s1, a) = run(s)
    f(a).run(s1)
  }
}

object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    def run = f
  }
}

object StateMonadPlayground extends App {

  case class Model(left: Int, right: Int)

  sealed trait Op
  case class Add(number: Int) extends Op
  case class Mul(number: Int) extends Op

  def doModel(operation: Op): State[Model, Op] = {
    val s1 = State[Model, Op] { model =>
      val (res,_) = doInt(operation).run(model.left)
      model.copy(left = res) -> operation
    }

    val s2 = State[Model, Op] { model =>
      val (res,_) = doInt(operation).run(model.right)
      model.copy(right = res) -> operation
    }


    val ss = s1 :: s2 :: Nil

    ss match {
      case h :: t =>
      case h :: Nil =>
      case Nil =>
    }

    val init = State[Model, Op] { model => model -> operation}

    ss.foldLeft (init) { (accu, act) =>
      val r = for {
        _ <- accu
        _ <- act
      } yield(operation)
      r
    }

//    for {
//      _ <- s1
//      _ <- s2
//    } yield(operation)

  }

  def doInt(operation: Op): State[Int, Op] = State { in =>
    val r = operation match {
      case Add(number) => in + number
      case Mul(number) => in * number
    }
    r -> operation
  }

  val res = for {
    _ <- doModel(Add(10))
    _ <- doModel(Add(20))
    _ <- doModel(Mul(2))
  } yield ()

  println(res.run(Model(10,10))._1)

}