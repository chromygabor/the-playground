package ui

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Terminated}

/**
  * Created by Gábor on 2016.09.28..
  */
class EventStreamActor extends Actor with ActorLogging {

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
    case event: EventEnvelope =>
      log.debug(s"Received: $event")
      subscribers.foreach(_ ! event)
  }

  override def receive: Actor.Receive = loop(Nil)
}

case class EventEnvelope(sender: ActorRef, event: Event) extends Event {
  val uid = UUID.randomUUID().toString

  override def toString(): String = {
    s"EventEnvelope[$uid]($event, $sender)"
  }
}

case class Subscribe(actor: ActorRef)

case class Unsubscribe(actor: ActorRef)
