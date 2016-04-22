package concept

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout
import concept.AppModel.{NewAppModelCreated, NewAppModel}
import concept.Child.{NewChildCreated, NewChild}
import concept.Repo.{GetRoot, GetState}
import rx.lang.scala.Subject
import akka.pattern._

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

trait DomainCommand

object EventRecord {

  object Uid {
    private[this] var _id = 0

    def apply(): Int = {
      _id = _id + 1
      _id
    }
  }

  def forAggregate[T](aggregateId: Int)(event: T): EventRecord[T] = EventRecord(aggregateId, event, false)

  def forNewAggregate[T](event: T): EventRecord[T] = EventRecord(Uid(), event, true)
}

case class EventRecord[T](aggregateId: Int, event: T, isNewAggregate: Boolean)

trait Aggregate {
  def behavior: Behavior[_ <: Aggregate]
}

case class CommandResult(validation: Validation[Any, EventRecord[_]], futureValue: Future[Validation[Any, EventRecord[_]]]) {
  def map(f: CommandResult => CommandResult): CommandResult = ???
}

trait Behavior[A <: Aggregate] {
  //  def validate(command: DomainCommand): CommandResult
  //
  //  def applyEvents(events: Seq[DomainEvent]): A
  //
  def applyEvent(event: Any): A
}

object Child {

  case object NewChild extends DomainCommand

  case class NewChildCreated()

  def validateNew(command: DomainCommand): CommandResult = {
    //    case NewChild =>
    //      CommandResult(
    //        events = SuccessfulValidation(Seq(EventRecord.forNewAggregate(NewChildCreated()))),
    //        futureValue = Future.successful(SuccessfulValidation(Seq()))
    //      )
    ???
  }

  def validate(aggregateId: Int, command: DomainCommand): CommandResult = {
    ???
  }
}

case class Child(value: Int)

object AppModel {

  case object NewAppModel extends DomainCommand

  case object NewAppModelCreated

  case class ChildAdded(childId: Int)

  def validateNew(command: DomainCommand): CommandResult = command match {
    case NewAppModel =>
      CommandResult(
        validation = SuccessfulValidation(Seq(
          EventRecord.forNewAggregate(NewAppModelCreated)
        )),
        futureValue = Future.successful(SuccessfulValidation(Seq()))
      )
    case _ => ???
  }

  def validate(aggregateId: Int, command: DomainCommand): CommandResult = command match {
    case _ => ???
    //    case NewChild =>
    //      Child.validateNew(NewChild)
    //        .map { _ =>
    //          CommandResult(
    //            events = SuccessfulValidation(Seq()),
    //            futureValue = Future.successful(
    //              SuccessfulValidation(Seq(
    //                EventRecord.forAggregate(aggregateId)(NewChildCreated())
    //              ))
    //            )
    //          )
    //        }
  }
}

case class AppModel(children: Seq[Child]) extends Aggregate {
  override def behavior: Behavior[_ <: Aggregate] = ???
}


object Repo {
  case object GetState
  case object GetRoot
}

class Repo extends Actor {

  import context._

  def handle(events: Seq[EventRecord[_]]): Unit = {
    val newState = events.foldLeft(state) { (state, event) =>
      val res = if (event.isNewAggregate) {
        event.event match {
          case NewAppModelCreated => Some(event.aggregateId -> AppModel(Seq()))
          case _ => None
        }
      } else {
        Some(event.aggregateId -> state(event.aggregateId).behavior.applyEvent(event))
      }

      res.map { case (aggregateId, aggregate) =>
        state.updated(aggregateId, aggregate)
      }.getOrElse(state)
    }
    
  }
  
  
  override def receive: Receive = receive(Map.empty)

  def receive(state: Map[Int, Aggregate]): Receive = {
    case events: Seq[EventRecord[_]] =>
      val newState = events.foldLeft(state) { (state, event) =>
        val res = if (event.isNewAggregate) {
          event.event match {
            case NewAppModelCreated => Some(event.aggregateId -> AppModel(Seq()))
            case _ => None
          }
        } else {
          Some(event.aggregateId -> state(event.aggregateId).behavior.applyEvent(event))
        }

        res.map { case (aggregateId, aggregate) =>
          state.updated(aggregateId, aggregate)
        }.getOrElse(state)
      }
      become(receive(newState))
      sender ! events
      
    case GetState =>
      //println("GetState")
      sender ! state
  }
}

object ConceptApp extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  val system = ActorSystem()

  val repo = system.actorOf(Props(new Repo()))

  val commandPipe = Subject[CommandResult]

  implicit val timeout = Timeout(10, TimeUnit.SECONDS)

  def sendValidationToRepo(validation: Validation[Any, EventRecord[_]]): Unit = {
    validation match {
      case SuccessfulValidation(events) =>
        repo ? events
        (repo ? GetState).map(println)
      case FailedValidation(error) =>
        println(s"Error occurred $error, so the modification didn't happen")
    }    
  }
  
  val commandStream = commandPipe.foreach { command =>
    sendValidationToRepo(command.validation) 
//    command.futureValue.onComplete {
//      case Success(validation) =>
//        sendValidationToRepo(validation)
//      case Failure(error) =>
//        println(s"Unhandled error occurred: $error")
//    }
  }
    

//  for {
//    events <- repo ? AppModel.validateNew(NewAppModel)
//    state <- repo ? GetState
//  } {
//    println(s"$events, $state")
//  }
  //commandPipe.onNext()


  (repo ? GetRoot).map { appModel =>
    appModel match {
      case AppModel(children) =>
    }
  }
  
}
