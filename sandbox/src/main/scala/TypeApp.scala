object TypeApp extends App {

  abstract class Common[A <: Com : Manifest] {
    def acceptable[B](b: B): Boolean = {
      //this.getClass.isAssignableFrom(b.getClass)
      val m = manifest[A]
      m.runtimeClass.isAssignableFrom(b.getClass)
    }
  }

  trait Com
  class A extends Com
  class B extends Com
  
  case class Foo() extends Common[A]
  case class Bar() extends Common[B]
  

  val foo = Foo()
  val bar = Bar()

  val a = new A
  val a2 = new A
  val b = new B
  
  println(foo.acceptable(a))
  println(foo.acceptable(b))
  println(bar.acceptable(b))
  
}

