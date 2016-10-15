package ui

import akka.actor._
import akka.pattern._
import akka.util.Timeout

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by Gábor on 2016.09.28..
  */

case object Subscribed extends Command

case class ReceiveEvent(senderId: SenderId, event: Event) extends Command

object BehaviorId {
  def apply[T: Manifest](): BehaviorId = {
    val m = manifest[T]
    BehaviorId(s"${m.runtimeClass.getSimpleName}_${SenderId.uid}")
  }
}

case class BehaviorId(uid: String = SenderId.uid) extends SenderId

trait BehaviorRef {
  def !(event: Any): Unit
  def ?(event: Any): Future[Any]
}

trait Behavior[S] extends Actor with ActorLogging with Stash {
  private var _state: S = _
  private var _initialized = false

  def initialState: S

  private[this] var _executionContext: ExecutionContext = _
  implicit lazy val ec: ExecutionContext = _executionContext

  implicit def behaviorId2BehaviorRef(behaviorId: BehaviorId): BehaviorRef = new BehaviorRef {
    override def ?(event: Any): Future[Any] = behaviorContext.askBehavior(behaviorId, event)

    override def !(event: Any): Unit = behaviorContext.sendBehavior(behaviorId, event)
  }

  implicit def commandToList(cmd: Command): List[Event] = {
    val commandHandler = onCommand(_state)
    if (commandHandler.isDefinedAt(cmd)) commandHandler(cmd)
    else Nil
  }

  implicit def eventToListOfEvent(evt: Event): List[Event] = evt :: Nil

  case class OnSuccess[T](f: (S, T) => List[Event], result: T)

  def onSuccess[T](future: Future[T])(onComplete: (S, T) => List[Event])(implicit ec: ExecutionContext): Unit = {
    future.onComplete {
      case Success(r) => self ! OnSuccess(onComplete, r)
      case Failure(error) =>
    }
  }

  def onSuccess[T](f: => T)(onComplete: (S, T) => List[Event])(implicit ec: ExecutionContext): Unit = {
    onSuccess(Future(f))(onComplete)
  }

  def processEvent(event: Event, context: Context): Unit = {
    if (event == Initialized) {
      _initialized = true
    }
    val eventListener = onEvent(_state, context)
    if (eventListener.isDefinedAt(event)) {
      val newState = eventListener(event)
      if (newState != _state) {
        _state = newState
      }
    }
  }

  def processCommand(c: Command, behaviorContext: Context, behaviorId: BehaviorId): Unit = {
    val commandListener = onCommand(_state)
    if (commandListener.isDefinedAt(c)) {
      commandListener(c).foreach { event =>
        log.info(s"Publishing event: $event")
        behaviorContext.eventStream ! behaviorId -> event
      }
    }
  }

  private[this] var _behaviorContext: Option[(Context, BehaviorId)] = None
  def behaviorContext = _behaviorContext.map(_._1).getOrElse(throw new IllegalAccessException("BehaviorContext isn't set yet"))
  def behaviorId = _behaviorContext.map(_._2).getOrElse(throw new IllegalAccessException("BehaviorContext isn't set yet"))

  val receive: Receive = {
    case InitBehavior(behaviorContext, behaviorId) =>
      behaviorContext.eventStream ! Subscribe(self)
      _executionContext = behaviorContext.executionContext

      unstashAll()
      _state = initialState

      _behaviorContext = Some((behaviorContext, behaviorId))
      if (!_initialized) self ! Init

    case e if _behaviorContext.isEmpty =>
      stash()
    case OnSuccess(f, result) =>
      f(_state, result).foreach { event =>
        behaviorContext.eventStream ! behaviorId -> event
      }

    case c: Command =>
      processCommand(c, behaviorContext, behaviorId)

    case Envelope(senderId, event, _) if senderId == behaviorId =>
      processEvent(event, behaviorContext)

    case Envelope(senderId, event, _) =>
      processCommand(ReceiveEvent(senderId, event), behaviorContext: Context, behaviorId)

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand(state: S): PartialFunction[Command, List[Event]]

  def onEvent(state: S, context: Context): PartialFunction[Event, S]
}

case class InitBehavior(behaviorContext: Context, behaviorId: BehaviorId)


class Context(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, implicit val executionContext: ExecutionContext, implicit val timeout: Timeout) {
  case class PutBehavior(behaviorId: BehaviorId, props: Props)

  case class GetBehavior(behaviorId: BehaviorId)

  val behaviorContext = this

  case class Send(behaviorId: BehaviorId, event: Any)
  case class Ask(behaviorId: BehaviorId, event: Any)

  private[this] val store = actorRefFactory.actorOf(Props(new Actor with ActorLogging {
    var behaviors = Map.empty[BehaviorId, ActorRef]

    override def receive: Actor.Receive = {
      case PutBehavior(behaviorId, props) =>
        if (behaviors.contains(behaviorId)) {
          log.debug(s"$behaviorId is existing already")
          sender() ! akka.actor.Status.Failure(new IllegalArgumentException(s"The behavior with ID #$behaviorId is already existing"))
        } else {
          log.debug(s"$behaviorId is not existing yet")
          val actorRef = actorRefFactory.actorOf(props, s"$behaviorId")
          actorRef ! InitBehavior(behaviorContext, behaviorId)
          context.watch(actorRef)
          behaviors = behaviors.updated(behaviorId, actorRef)
          sender() ! behaviorId
        }
      case Terminated(terminatedActor) =>
        behaviors = behaviors.filterNot { case (behaviorId, actorRef) => actorRef == terminatedActor }
      case Send(behaviorId: BehaviorId, event: Any) =>
        behaviors.get(behaviorId).foreach(_ ! event)
      case Ask(behaviorId: BehaviorId, event: Any) =>
        behaviors.get(behaviorId).map (actor => actor.ask(event)) match {
          case Some(f) => f.pipeTo(sender())
          case None => sender() ! akka.actor.Status.Failure(new IllegalArgumentException(s"Behavior with ID: #$behaviorId doesn't exist"))
        }
    }

  }), "BehaviorStore")


  def create(behaviorId: BehaviorId, creator: => Behavior[_]): Future[BehaviorId] = create(behaviorId, Props(creator))

  def create(behaviorId: BehaviorId, props: Props): Future[BehaviorId] = {
    (store ? PutBehavior(behaviorId, props)).collect {
      case b: BehaviorId => b
    }
  }

  def askBehavior(behaviorId: BehaviorId, event: Any): Future[Any] = {
    store ? Ask(behaviorId, event)
  }

  def sendBehavior(behaviorId: BehaviorId, event: Any): Unit = {
    store ! Send(behaviorId, event)
  }
}
