package concept

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import concept.AppModel.NewChildAdded
import concept.Child.{NewChild, NewChildCreated}
import concept.EventRecord.{Uid, Channel, CommandResult}
import rx.lang.scala.Subject

import scala.concurrent.Future

trait DomainCommand

object EventRecord {

  type CommandResult = Validation[Any, Any]

  type Channel = CommandResult => Any

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

case class Ref[T](uid: Int) {
  //def apply(): T = ???
}

object Ref {

  private[this] var _id = 0

  private[this] def next(): Int = {
    _id = _id + 1
    _id
  }

  def of[T]: Ref[T] = Ref[T](next())
}

object Child {

  case object NewChild extends DomainCommand

  case class NewChildCreated(ref: Ref[Child]) {
    def apply() = new Child(0)
  }

  def validateNew(command: DomainCommand, channel: Channel): CommandResult = {
    command match {
      case NewChild => SuccessfulValidation(Seq(NewChildCreated(Ref.of[Child])))
    }
  }
}

case class Child(value: Int)

object AppModel {

  case class NewChildAdded(ref: Ref[Child])

}

case class AppModel(children: Seq[Ref[Child]] = Seq.empty) {

  import scala.concurrent.ExecutionContext.Implicits.global

  def validate(command: DomainCommand, channel: Channel): CommandResult = command match {
    case NewChild =>
      val validateNew = Child.validateNew(NewChild, channel)
      validateNew.foreach { events =>
        Future {
          events.collectFirst { case NewChildCreated(ref) => ref } match {
            case Some(ref) =>
              channel(SuccessfulValidation(Seq(NewChildAdded(ref))))
            case None =>
              channel(FailedValidation(new IllegalStateException("No new ref came from the Child")))
          }
        }
      }
      validateNew
    case _ => ???
  }

  def handle(event: Any): AppModel = event match {
    case NewChildAdded(ref) => copy(children :+ ref)
    case _ => this
  }


}


object Repo {

  case object GetState

  case object GetRoot

}

class Repo {

  case class Aggregate[T](ref: Ref[T], aggregate: T)
  
  var _state: Seq[Aggregate[_]] = Seq(Aggregate(Ref[AppModel](0), AppModel()))

  def handle(events: Seq[Any]): Seq[Any] = {
    _state = events.foldLeft(_state) { (state, event) =>
      event match {
        case e@NewChildCreated(ref) =>
          _state = _state :+ Aggregate(ref, e.apply())
      }
      //state.handle(event)
      state
    }
    events
  }

  def root = byId(Ref[AppModel](0)).getOrElse(throw new IllegalStateException("No root initialized"))

  def byId[T](ref: Ref[T]): Option[T] = {
    _state.find(in => in.ref == ref).map(_.aggregate.asInstanceOf[T])
  }
}

object ConceptApp extends App {
  val system = ActorSystem()

  val repo = new Repo()
  val commandPipe = Subject[CommandResult]()

  implicit val timeout = Timeout(10, TimeUnit.SECONDS)

  def sendValidationToRepo(validation: Validation[Any, Any]): Unit = {
    validation match {
      case SuccessfulValidation(events) =>
        val newEvents = repo.handle(events)
        println(s"handled events: $newEvents, new state: ${repo.root}")
      case FailedValidation(error) =>
        println(s"Error occurred $error, so the modification didn't happen")
    }
  }

  val commandStream = commandPipe.foreach { command =>
    println(s"Command received: $command")
    sendValidationToRepo(command)
  }


  //  for {
  //    events <- repo ? AppModel.validateNew(NewAppModel)
  //    state <- repo ? GetState
  //  } {
  //    println(s"$events, $state")
  //  }
  
  commandPipe.onNext(repo.root.validate(NewChild, commandPipe.onNext))
  Thread.sleep(1000)

}
