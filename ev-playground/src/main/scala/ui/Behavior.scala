package ui

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, ActorRefFactory, Props, Stash}
import ui.BehaviorContext.InitBehaviorContext

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by Gábor on 2016.09.28..
  */
trait Behavior[S] extends Actor with ActorLogging with Stash {

  private var _state: S = _
  private var _stateChangeIsAllowed = false

  def state: S = _state

  def state_=(newState: S): Unit = {
    if (_stateChangeIsAllowed) {
      _state = newState
    } else {
      sys.error("State change is not allowed now")
    }
  }

  type EventReceive = BehaviorContext.EventReceive

  private[this] var _executionContext: ExecutionContext = _
  implicit lazy val ec: ExecutionContext = _executionContext

  case class OnSuccess[T](f: T => List[Event], result: T)

  def onSuccess[T](future: => T)(f: T => List[Event])(implicit ec: ExecutionContext): Unit = {
    Future(future).onComplete {
      case Success(r) => self ! OnSuccess(f, r)
      case Failure(error) =>
    }
  }


  def receive: Receive = {
    case InitBehaviorContext(f) =>
      val behaviorContext = f(context, self)
      behaviorContext.eventStream ! Subscribe(self)
      _executionContext = behaviorContext.executionContext

      unstashAll()
      context.become(contextInitialized(behaviorContext))
      self ! Init
    case _ =>
      stash()
  }

  def contextInitialized(behaviorContext: BehaviorContext): Receive = {
    case OnSuccess(f, result) =>
      _stateChangeIsAllowed = true
      val events = f(result).map(EventEnvelope(self, _))
      _stateChangeIsAllowed = false
      events.foreach(behaviorContext.eventStream ! _)
    case c: Command if onCommand.isDefinedAt(c) =>
      _stateChangeIsAllowed = true
      val events = onCommand(c).map(EventEnvelope(self, _))
      _stateChangeIsAllowed = false
      events.foreach(behaviorContext.eventStream ! _)
    case EventEnvelope(sender, event) if sender == self && onEvent.isDefinedAt(event) =>
      onEvent(event)

    case e: EventEnvelope if onEvent.isDefinedAt(e) =>
      onEvent(e)

    case c: Command =>
    case e: EventEnvelope =>

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand: PartialFunction[Command, List[Event]] = new PartialFunction[Command, List[Event]] {
    override def isDefinedAt(x: Command): Boolean = false

    override def apply(v1: Command): List[Event] = throw new IllegalAccessError("Not defined")
  }

  def onEvent = new PartialFunction[Event, Command] {
    override def isDefinedAt(x: Event): Boolean = false

    override def apply(v1: Event): Command = throw new IllegalAccessError("Not defined")
  }
}

object BehaviorContext {
  type EventReceive = PartialFunction[Event, Unit]

  case class InitBehaviorContext(f: (ActorRefFactory, ActorRef) => BehaviorContext)

}

class BehaviorContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, val executionContext: ExecutionContext) {
  def behaviorOf(creator: Props): ActorRef = {
    val behavior = actorRefFactory.actorOf(creator, s"Behavior-${UUID.randomUUID().toString}")
    behavior ! InitBehaviorContext(new BehaviorContext(_, _, eventStream, executionContext))
    behavior
  }

}
