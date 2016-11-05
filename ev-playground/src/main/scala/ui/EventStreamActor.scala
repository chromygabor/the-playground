package ui

import java.util.UUID

import akka.actor.{ActorLogging, ActorRef, Terminated}
import akka.persistence.{PersistentActor, Recovery, RecoveryCompleted}

import scala.collection.immutable.Queue
import scala.concurrent.Future

/**
  * Created by Gábor on 2016.09.28..
  */

case class Replay(f: (Envelope => Boolean), actorRef: ActorRef)

class EventStreamActor extends PersistentActor with ActorLogging {
  override def persistenceId = "EventStream"

  var lastSentId = 0L

  override def recovery = Recovery.none

  var _events: Seq[Envelope] = Queue()

  def replay(pred: (Envelope) => Boolean, actorRef: ActorRef): Unit = {
    import context.dispatcher
    Future {
      _events.filter(pred)
    }.foreach(_.foreach(actorRef ! _))
  }

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
    case (sender: ServiceId, e: Event) =>
      val event = Envelope(sender, e, lastSentId + 1)
      event.event match {
        case e: PersistentEvent =>
          persist(event) { event =>
            log.debug(s"Event persisted: $event")
            _events = _events :+ event
            subscribers.foreach(_ ! event)
          }
        case e =>
          log.info(s"Received: $event")
          subscribers.foreach(_ ! event)
      }
      lastSentId = event.id

    case Replay(pred, actorRef) => replay(pred, actorRef)
  }

  override def receiveRecover: Receive = {
    case e: Envelope =>
      lastSentId = e.id
      _events = _events :+ e
      log.debug(s"Change lastSentId to $lastSentId")
    case RecoveryCompleted =>
      log.debug("RecoveryCompleted")
  }

  override def receiveCommand: Receive = loop(Nil)
}

case class Subscribe(actor: ActorRef)

case class Unsubscribe(actor: ActorRef)

object ServiceId {
  def uid = UUID.randomUUID().toString.take(10)

  def apply(uid: String) = new ServiceId(uid)

  def apply[T: Manifest]: ServiceId = {
    val m = manifest[T]
    new ServiceId(s"${m.runtimeClass.getSimpleName}_${ServiceId.uid}")
  }
  def singleton[T: Manifest]: ServiceId = {
    val m = manifest[T]
    new ServiceId(s"${m.runtimeClass.getSimpleName}")
  }

}

class ServiceId(val uid: String) {
  override def toString: String = "ActorId(\""+uid+"\")"
  override def equals(other: Any): Boolean = other match {
    case cId: ServiceId if this.uid.equals(cId.uid) => true
    case _ => false
  }
  override def hashCode: Int = {
    val prime = 31
    prime * uid.hashCode
  }
}

case class Envelope (sender: ServiceId, event: Event, id: Long) {
  override def toString: String = {
    s"EventEnvelope[${id}](sender=$sender, event=$event)"
  }
}
