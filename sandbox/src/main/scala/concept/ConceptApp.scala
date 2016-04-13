package concept

import concept.DomainValidation.DomainValidation
import rx.lang.scala.Subject

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DomainValidation {
  type DomainValidation = Validation[DomainEvent, DomainEvent]
}

trait DomainCommand

trait DomainEvent

trait Aggregate {
  def behavior: Behavior[_ <: Aggregate]
}

case class CommandResult ( events: DomainValidation, futureValue: Future[DomainValidation])

trait Behavior[A <: Aggregate] {
  def validate(command: DomainCommand): CommandResult

  def applyEvents(events: Seq[DomainEvent]): A

  def applyEvent(event: DomainEvent): A
}

case class AddValue(value: Int) extends DomainCommand

case class ValueAdded(value: Int) extends DomainEvent

case class Error(errorMessage: String) extends DomainEvent

object Product {

  def behavior(model: Product) = new Behavior[Product] {
    override def validate(command: DomainCommand): CommandResult = {
      command match {
        case AddValue(value) if value < 10 => 
          CommandResult(SuccessfulValidation(Seq(ValueAdded(value))), Future.successful(SuccessfulValidation(Seq())))
        case AddValue(value) if value >= 10 && value <= 20 =>
          CommandResult(FailedValidation(Error(s"Value shouldn't be between 10 and 20: $value")), Future.successful(SuccessfulValidation(Seq())))
        
        case AddValue(value) if value > 20 => 
          val f = Future {
            Thread.sleep(1000)
            SuccessfulValidation(Seq(ValueAdded(value)))
          }.recover{
            case e => FailedValidation(Error(s"Error occurred ${e.getMessage}"))
          }
          CommandResult(SuccessfulValidation(Seq()), f)
      }
    }

    def applyEvents(events: Seq[DomainEvent]): Product = events.foldLeft(model) ( (prevState, event) => prevState.behavior.applyEvent(event))
    
    override def applyEvent(event: DomainEvent): Product = event match {
      case ValueAdded(value) => model.copy(value = value)
      case _ => model
    }
  }

}

trait Validation[+F, +S] {
  def flatMap[FB >: F, SB >: S](f: Seq[S] => Validation[FB, SB]): Validation[FB, SB] = this match {
    case SuccessfulValidation(events) => f(events) match {
        case SuccessfulValidation(newEvents) => SuccessfulValidation(events ++ newEvents)
        case e@FailedValidation(_) => e
    }
    case e@FailedValidation(_) => e
  }

  def map[SB >: S](f: Seq[S] => Seq[SB]): Validation[F, SB] = this match {
    case SuccessfulValidation(events) =>  
      SuccessfulValidation(f(events))
    case e@FailedValidation(_) => e
  }
  
  def isFailed = !isSuccess
  def isSuccess = this match {
    case SuccessfulValidation(_) => true
    case FailedValidation(_) => false
  }
  
}
case class SuccessfulValidation[S](events: Seq[S]) extends Validation[Nothing, S]
case class FailedValidation[F](failure: F) extends Validation[F, Nothing]

class ValidationTest {
  val validation1: DomainValidation = for {
    _ <- SuccessfulValidation(Seq(ValueAdded(10)))
    _ <- FailedValidation(Error("Some error"))
    v3 <- SuccessfulValidation(Seq(ValueAdded(30)))
  } yield {
      v3
    }

  assert(validation1 == FailedValidation(Error("Some error")))

  val validation2: DomainValidation = for {
    _ <- SuccessfulValidation(Seq(ValueAdded(10)))
    _ <- SuccessfulValidation(Seq(ValueAdded(20)))
    v3 <- SuccessfulValidation(Seq(ValueAdded(30)))
  } yield {
      v3
    }

  assert(validation2 == SuccessfulValidation(Seq(
    ValueAdded(10),
    ValueAdded(20),
    ValueAdded(30)
  )
  ))
  
}

case class Child(value: Int)
case class AppModel(children: Seq[Child])

case class Product(name: String, value: Int = 0) extends Aggregate {
  val behavior: Behavior[Product] = Product.behavior(this)
}

object ProductRepository {
  var _products: Map[Int, Product] = Map.empty
  
  def applyEvents(events: Seq[DomainEvent]): Unit = {
    
  }
}

object ConceptApp extends App {

  val state0 = Product("Product0")

  val commandStream = Subject[CommandResult]
  
  val ss = commandStream.scan(state0) { (prevState, command) =>
    val validation = command.events
    validation match {
      case SuccessfulValidation(events) =>
        prevState.behavior.applyEvents(events)
      case FailedValidation(event) =>
        prevState.behavior.applyEvent(event)
    }
  }
  ss.foreach(println)
  
  val commandResult = state0.behavior.validate(AddValue(100))

  println("Emmiting this CommandResult: " + commandResult)
  commandStream.onNext(commandResult)

  //  events.foreach(println)
  //  events.recover {
  //    case e => e.printStackTrace()
  //  }
  //  
  //  events.map {
  //    _.foldLeft(state0) { (state, event) =>
  //      state.behavior.applyEvent(event)
  //    }
  //  }.foreach(println)
  //      

}
