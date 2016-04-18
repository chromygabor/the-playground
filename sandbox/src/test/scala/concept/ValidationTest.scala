package concept

import concept.Validation
import org.scalatest.{Matchers, FunSpecLike}
import Matchers._

/**
 * Created by cry on 2016.04.13..
 */
class ValidationTest extends FunSpecLike {
  trait DomainEvent
  
  type DomainValidation = Validation[DomainEvent, DomainEvent]

  case class ValueAdded(value: Int) extends DomainEvent
  case class Error(message: String) extends DomainEvent
  
  describe("Validation") {
    it("aggregate all the events") {
      val validation2: DomainValidation = for {
        _ <- SuccessfulValidation(Seq(ValueAdded(10)))
        _ <- SuccessfulValidation(Seq(ValueAdded(20)))
        v3 <- SuccessfulValidation(Seq(ValueAdded(30)))
      } yield {
          v3
        }

      validation2 should equal(SuccessfulValidation(Seq(
        ValueAdded(10),
        ValueAdded(20),
        ValueAdded(30)
      ))) 
    }
    
    it("stops at the first error, and propagates the error only") {
      val validation1: DomainValidation = for {
        _ <- SuccessfulValidation(Seq(ValueAdded(10)))
        _ <- FailedValidation(Error("Some error"))
        v3 <- SuccessfulValidation(Seq(ValueAdded(30)))
      } yield {
          v3
        }

      validation1 should equal(FailedValidation(Error("Some error")))
    }
    
    it("have to be covariant") {

      class BaseClass
      class ChildClass extends BaseClass
      class AnotherClass

      val s1: Validation[Nothing, BaseClass] = SuccessfulValidation(Seq(new BaseClass))
      // val s2: Validation[Nothing, BaseClass] = SuccessfulValidation(Seq(new AnotherClass)) //This should not compile
      val s3: Validation[Nothing, BaseClass] = SuccessfulValidation(Seq(new ChildClass))
      
    }
  }
  

  
}
