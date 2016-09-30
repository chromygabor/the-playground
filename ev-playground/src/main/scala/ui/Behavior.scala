package ui

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, ActorRefFactory, Props, Stash}
import ui.BehaviorContext.InitBehaviorContext

import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.util.{Try, Failure, Success}

/**
  * Created by Gábor on 2016.09.28..
  */
trait Behavior[S] extends Actor with ActorLogging with Stash {

  private var _state: S = _

  type EventReceive = BehaviorContext.EventReceive

  private[this] var _executionContext: ExecutionContext = _
  implicit lazy val ec: ExecutionContext = _executionContext

  case class OnSuccess[T](f: (S,T) => List[Event], result: T)

  def onSuccess[T](future: Future[T])(onComplete: (S,T) => List[Event])(implicit ec: ExecutionContext): Unit = {
    future.onComplete {
      case Success(r) => self ! OnSuccess(onComplete, r)
      case Failure(error) =>
    }

  }

  def onSuccess[T](f: => T)(onComplete: (S,T) => List[Event])(implicit ec: ExecutionContext): Unit = {
    onSuccess(Future(f))(onComplete)
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
//    case OnSuccess(f, result) =>
//      val events = f(result).map(EventEnvelope(self, _))
//      events.foreach(behaviorContext.eventStream ! _)
    case c: Command =>
      val commandListener = onCommand(_state)
      if(commandListener.isDefinedAt(c)) {
        val events = commandListener(c).map(EventEnvelope(self, _))
        events.foreach(behaviorContext.eventStream ! _)
      }
    case EventEnvelope(sender, event) if sender == self =>
      val eventListener = onEvent(_state)
      if(eventListener.isDefinedAt(event)) _state = eventListener(event)

    case event: EventEnvelope =>
      val eventListener = onEvent(_state)
      if(eventListener.isDefinedAt(event)) _state = eventListener(event)

    case c: Command =>
    case e: EventEnvelope =>

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand(state: S): PartialFunction[Command, List[Event]] = new PartialFunction[Command, List[Event]] {
    override def isDefinedAt(x: Command): Boolean = false

    override def apply(v1: Command): List[Event] = throw new IllegalAccessError("Not defined")
  }

  def onEvent(state: S) = new PartialFunction[Event, S] {
    override def isDefinedAt(x: Event): Boolean = false

    override def apply(v1: Event): S = throw new IllegalAccessError("Not defined")
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
