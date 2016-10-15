package ui

import java.util.UUID

import akka.actor.{ActorLogging, ActorRef, Terminated}
import akka.persistence.{PersistentActor, RecoveryCompleted}

/**
  * Created by Gábor on 2016.09.28..
  */

class EventStreamActor extends PersistentActor with ActorLogging {
  override def persistenceId = "EventStream"

  var lastSentId = 0L

  def loop(subscribers: List[ActorRef]): Receive = {
    case Subscribe(subscriber) =>
      log.debug(s"Subscribe from: $subscriber")
      context.watch(subscriber)
      context.become(loop(subscriber :: subscribers))
    case Terminated(subscriber) =>
      log.debug(s"Terminated from: $subscriber")
      context.become(loop(subscribers.filterNot(_ == subscriber)))
    case Unsubscribe(subscriber) =>
      log.debug(s"Unsubscribe from: $subscriber")
      context.become(loop(subscribers.filterNot(_ == subscriber)))
    case (sender: SenderId, e: Event) =>
      val event = Envelope(sender, e, lastSentId + 1)
      event.event match {
        case e: PersistentEvent =>
          persist(event) { event =>
            log.debug(s"Event persisted: $event")
            subscribers.foreach(_ ! event)
          }
        case e =>
          log.info(s"Received: $event")
          subscribers.foreach(_ ! event)
      }
      lastSentId = event.id

  }

  override def receiveRecover: Receive = {
    case e: Envelope =>
      lastSentId = e.id
      log.debug(s"Change lastSentId to $lastSentId")
    case RecoveryCompleted =>
      log.debug("RecoveryCompleted")
  }

  override def receiveCommand: Receive = loop(Nil)
}

case class Subscribe(actor: ActorRef)

case class Unsubscribe(actor: ActorRef)

object SenderId {
  def uid = UUID.randomUUID().toString.take(10)
}

trait SenderId {
  def uid: String

  override def toString: String = uid
}

case class Envelope (sender: SenderId, event: Event, id: Long) {
  override def toString: String = {
    s"EventEnvelope[${id}](sender=$sender, event=$event)"
  }
}
