package com.chromy.reactiveui

import org.scalatest.FunSpecLike

/**
 * Created by chrogab on 2015.08.10..
 */
class JavaFXFactoryTest extends FunSpecLike {

  trait BaseMapper {
    type M
  }

  trait Mapper[A] extends BaseMapper {
    type M = A
  }

  def testMethod[A <: Mapper[_] : Manifest](): Unit = {
    import reflect.runtime.{currentMirror => mirror}
    import scala.reflect.runtime.universe._

    val m = typeOf[A].member("M": TypeName)
    val tpe = typeOf[A]

    println(m.asType.toTypeIn(tpe))
  }

  case class TestClass()

  describe("Component") {
    it("should be instantiated by an already exitsing component") {
      testMethod[Mapper[TestClass]]
    }
  }
}
