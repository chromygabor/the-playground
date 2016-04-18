package concept

import scala.concurrent.Future

trait DomainCommand

object EventRecord {

  object Uid {
    private[this] var _id = 0

    def apply(): Int = {
      _id = _id + 1
      _id
    }
  }

  def forAggregate[T](aggregateId: Int)(event: T): EventRecord[T] = EventRecord(aggregateId, event)

  def forNewAggregate[T](event: T): EventRecord[T] = forAggregate(Uid())(event)
}

case class EventRecord[T](aggregateId: Int, event: T)

trait Aggregate {
  def behavior: Behavior[_ <: Aggregate]
}

case class CommandResult(events: Validation[Any, EventRecord[_]], futureValue: Future[Validation[Any, EventRecord[_]]]) {
  def map(f: CommandResult => CommandResult): CommandResult = ???
}

trait Behavior[A <: Aggregate] {
  //  def validate(command: DomainCommand): CommandResult
  //
  //  def applyEvents(events: Seq[DomainEvent]): A
  //
  //  def applyEvent(event: DomainEvent): A
}

case object NewAppModel extends DomainCommand

case object NewChild extends DomainCommand

case class NewChildCreated()

case class NewAppModelCreated()

case class ChildAdded(childId: Int)

object Child {

  def validateNew(command: DomainCommand): CommandResult = {
    case NewChild =>
      CommandResult(
        events = SuccessfulValidation(Seq(EventRecord.forNewAggregate(NewChildCreated()))),
        futureValue = Future.successful(SuccessfulValidation(Seq()))
      )
  }

  def validate(aggregateId: Int, command: DomainCommand): CommandResult = {
    ???
  }
}

case class Child(value: Int)

object AppModel {
  def validateNew(command: DomainCommand): CommandResult = {
    case NewAppModel =>
      CommandResult(
        events = SuccessfulValidation(Seq(
          EventRecord.forNewAggregate(NewAppModelCreated())
        )),
        futureValue = Future.successful(SuccessfulValidation(Seq()))
      )
  }

  def validate(aggregateId: Int, command: DomainCommand): CommandResult = command match {
    case NewChild =>
      Child.validateNew(NewChild)
        .map { _ =>
          CommandResult(
            events = SuccessfulValidation(Seq()),
            futureValue = Future.successful(
              SuccessfulValidation(Seq(
                EventRecord.forAggregate(aggregateId)(NewChildCreated())
              ))
            )
          )
        }
  }
}

case class AppModel(children: Seq[Child])

object ConceptApp extends App {

}
