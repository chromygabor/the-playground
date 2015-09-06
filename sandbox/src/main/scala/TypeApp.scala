object TypeApp extends App {

  trait A

  case class B[T](value: T, f: (T => Boolean)) extends A

  def dummy(input: A) = input match {
    case b: B[_] =>
      println(b.f(b.value))
    case _ => println("else")
  }



  dummy(B[Int](10, {in => true}))
}

