package ui

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, ActorRefFactory, Props, Stash}
import ui.BehaviorContext.InitBehaviorContext

import scala.concurrent.ExecutionContext

/**
  * Created by Gábor on 2016.09.28..
  */
trait Behavior extends Actor with ActorLogging with Stash {

  type EventReceive = BehaviorContext.EventReceive

  private[this] var _executionContext: ExecutionContext = _
  implicit lazy val ec: ExecutionContext = _executionContext

  def receive: Receive = {
    case InitBehaviorContext(f) =>
      val behaviorContext = f(context, self)
      behaviorContext.eventStream ! Subscribe(self)

      _executionContext = behaviorContext.executionContext

      unstashAll()
      context.become(contextInitialized(behaviorContext))
    case _ =>
      stash()
  }

  def contextInitialized(behaviorContext: BehaviorContext): Receive = {
    case c: Command if onCommand(behaviorContext.commandContext).isDefinedAt(c) =>
      val r = onCommand(behaviorContext.commandContext)(c)
      behaviorContext.commandContext.fire(r)

    case EventEnvelope(sender, event) if sender == self && onEvent.isDefinedAt(event) =>
      onEvent(event)

    case e: EventEnvelope if onEvent.isDefinedAt(e) =>
      onEvent(e)

    case c: Command =>
    case e: EventEnvelope =>

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand(context: CommandContext): PartialFunction[Command, List[Event]] = new PartialFunction[Command, List[Event]] {
    override def isDefinedAt(x: Command): Boolean = false

    override def apply(v1: Command): List[Event] = throw new IllegalAccessError("Not defined")
  }

  def onEvent: EventReceive = new PartialFunction[Event, Unit] {
    override def isDefinedAt(x: Event): Boolean = false

    override def apply(v1: Event): Unit = throw new IllegalAccessError("Not defined")
  }
}

object BehaviorContext {
  type EventReceive = PartialFunction[Event, Unit]

  case class InitBehaviorContext(f: (ActorRefFactory, ActorRef) => BehaviorContext)

}

case class CommandContext(owner: ActorRef, eventStream: ActorRef) {
  def fire(event: Event): Unit = eventStream.tell(EventEnvelope(owner, event), owner)

  def fire(events: Seq[Event]): Unit = events.foreach(event => eventStream.tell(EventEnvelope(owner, event), owner))
}

class BehaviorContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, val executionContext: ExecutionContext) {
  def behaviorOf(creator: Props): ActorRef = {
    val behavior = actorRefFactory.actorOf(creator, s"Behavior-${UUID.randomUUID().toString}")
    behavior ! InitBehaviorContext(new BehaviorContext(_, _, eventStream, executionContext))
    behavior
  }

  lazy val commandContext = new CommandContext(owner, eventStream)
}
