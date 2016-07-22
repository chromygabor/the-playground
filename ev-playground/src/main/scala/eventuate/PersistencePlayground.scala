package eventuate

import akka.NotUsed
import akka.actor.{Props, ActorSystem}
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import akka.persistence.query.journal.leveldb.scaladsl.LeveldbReadJournal
import akka.persistence.{SnapshotOffer, PersistentActor}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import com.typesafe.config.ConfigFactory

/**
 * Created by chrogab on 2016.07.22..
 */

case class Cmd(data: String)

case class Evt(data: String)

object PersistencePlayground extends App {
  val port = 2551
  val config = ConfigFactory.parseString(s"akka.persistence.journal.leveldb.dir=target/journal/$port").
    withFallback(ConfigFactory.load())

  val system = ActorSystem("test-system", config)
  implicit val mat = ActorMaterializer()(system)

  val queries = PersistenceQuery(system).readJournalFor[LeveldbReadJournal](LeveldbReadJournal.Identifier)

  val events: Source[Any, NotUsed] = queries.eventsByPersistenceId("user-1")

  events.runForeach(event => println(s"Event: $event"))
  
  val example = system.actorOf(Props(new ExamplePersistenceActor("user-1")))
  example ! "print"
  example ! Cmd("Command")
  example ! Cmd("Command")
  example ! Cmd("Command")
  example ! Cmd("Command")
  example ! "print"
}

case class ExampleState(events: List[String] = Nil) {
  def updated(evt: Evt): ExampleState = copy(evt.data :: events)

  def size: Int = events.length

  override def toString: String = events.reverse.toString
}

class ExamplePersistenceActor(val persistenceId: String) extends PersistentActor {
  var state = ExampleState()

  def updateState(event: Evt): Unit =
    state = state.updated(event)

  def numEvents =
    state.size

  val receiveRecover: Receive = {
    case evt: Evt => updateState(evt)
    case SnapshotOffer(_, snapshot: ExampleState) => state = snapshot
  }

  val receiveCommand: Receive = {
    case Cmd(data) =>
      persist(Evt(s"${data}-${numEvents}"))(updateState)
    case "snap" => saveSnapshot(state)
    case "print" => println(state)
  }
}
