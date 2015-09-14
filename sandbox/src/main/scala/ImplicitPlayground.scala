
import scala.annotation.implicitNotFound

/**
 * Created by cry on 2015.09.09..
 */
object ImplicitPlayground extends App{
  import MyMath.NumberLike

  implicit object NumberLikeInt extends NumberLike[Int]{
    override def add(x: Int, y: Int): Int = x + y

    override def sub(x: Int, y: Int): Int = x - y
  }

  Foo.foo(1, 2)
}

object Foo {
  import MyMath.NumberLike

  def foo[T](x: T, y: T)(implicit ev: NumberLike[T]) = {
  }
}

object MyMath {
  @implicitNotFound("No member of type class NumberLike in scope for ${T}")
  trait NumberLike[T] {
    def add(x: T, y: T): T
    def sub(x: T, y: T): T
  }
}
