package concept

import org.scalatest.{Matchers, FunSpecLike}
import Matchers._

/**
 * Created by cry on 2016.04.13..
 */
class ValidationTest extends FunSpecLike {
  type DomainValidation = Validation[DomainEvent, DomainEvent]

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
  }
  

  
}
