package ui

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, ActorRefFactory, Props, Stash}
import akka.pattern._
import akka.persistence.{SnapshotOffer, PersistentActor}
import akka.util.Timeout
import rx.lang.scala.{Observable, Observer, Subject}
import ui.BehaviorContext.InitBehaviorContext

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by Gábor on 2016.09.28..
  */

case object PersistenceId
case object Subscribed extends Command

trait Behavior[S] extends PersistentActor with ActorLogging with Stash {
  private var _state: S = _
  private var _initialized = false

  def initialState: S
  def id: String

  lazy val persistenceId = id

  private[this] var _executionContext: ExecutionContext = _
  implicit lazy val ec: ExecutionContext = _executionContext

  implicit def commandToList(cmd: Command): List[Event] = {
    val commandHandler = onCommand(_state)
    if(commandHandler.isDefinedAt(cmd)) commandHandler(cmd)
    else Nil
  }
  implicit def eventToListOfEvent(evt: Event): List[Event] = evt :: Nil

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

  val receiveRecover: Receive = {
    case EventEnvelope(_, Initialized) =>
      println("Initialized should be recovered")
//    case evt: EventEnvelope =>
//      println(s"Received recovery: $evt")
//      _state = onEvent(_state)(evt)
//    case SnapshotOffer(_, snapshot: S) =>
//      _state = snapshot
  }

  val receiveCommand: Receive = {
    case InitBehaviorContext(f) =>
      val behaviorContext = f(context, self)
      behaviorContext.eventStream ! Subscribe(self)
      _executionContext = behaviorContext.executionContext

      unstashAll()
      _state = initialState
        context.become(contextInitialized(behaviorContext))
      sender() ! persistenceId
      if(!_initialized) self ! Init
    case _ =>
      stash()
  }

  def contextInitialized(behaviorContext: BehaviorContext): Receive = {
    case OnSuccess(f, result) =>
      val events = f(_state, result).map(EventEnvelope(self, _))
      events.foreach(behaviorContext.eventStream ! _)

    case c: Command =>
      val commandListener = onCommand(_state)
      if(commandListener.isDefinedAt(c)) {
        val events = commandListener(c).map(EventEnvelope(self, _))
        events.foreach {
          case e@EventEnvelope(_, _: PersistentEvent) =>
            println(s"Persisting $e")
            persist(e) (behaviorContext.eventStream ! _)
          case e => behaviorContext.eventStream ! e
        }
      }

    case EventEnvelope(`self`, event) =>
      if(event == Initialized) _initialized = true

      val eventListener = onEvent(_state)
      if(eventListener.isDefinedAt(event)) {
        val newState = eventListener(event)
        if(newState != _state ) {
          _state = newState
        }
      }

    case event: EventEnvelope =>
      val eventListener = onEvent(_state)
      if(eventListener.isDefinedAt(event)) {
        val newState = eventListener(event)
        if(newState != _state ) {
          _state = newState
        }
      }

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand(state: S): PartialFunction[Command, List[Event]] = new PartialFunction[Command, List[Event]] {
    override def isDefinedAt(x: Command): Boolean = false

    override def apply(v1: Command): List[Event] = throw new IllegalAccessError("Not defined")
  }

  def onEvent(state: S) = new PartialFunction[Event, S] {
    override def isDefinedAt(x: Event): Boolean = false

    override def apply(v1: Event): S = throw new IllegalStateException("Not defined")
  }
}

object BehaviorContext {
  case class InitBehaviorContext(f: (ActorRefFactory, ActorRef) => BehaviorContext)
}

class BehaviorContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, implicit val executionContext: ExecutionContext, implicit val timeout: Timeout) {
  def behaviorOf(creator: Props): ActorRef = {
    val behavior = actorRefFactory.actorOf(creator, s"Behavior-${UUID.randomUUID().toString}")
    behavior
  }

  def persistenceIdOf(actorRef: ActorRef): Future[String] = {
    (actorRef ? InitBehaviorContext(new BehaviorContext(_, _, eventStream, executionContext, timeout))).collect {
      case persistenceId: String => persistenceId
    }
  }

}
