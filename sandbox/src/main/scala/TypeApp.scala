object TypeApp extends App {
  trait Stepper[M] {

    def apply(a: Int, m: M): M
  }

  trait SteppableM[A,B <: Stepper[_]] {
    def step(a: Int)(implicit m: Manifest[B]) = {
      val l = m.typeArguments
      println(m)
    }
  }

  case class ConcreteM(dummy: Int) extends SteppableM[ConcreteM, ConcreteStepper.type]

  object ConcreteStepper extends Stepper[ConcreteM] {
    override def apply(a: Int, m: ConcreteM): ConcreteM = ???

  }

  ConcreteM(1).step(2)
}

