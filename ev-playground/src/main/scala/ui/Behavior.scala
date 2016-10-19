package ui

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import ui.Context.{Ask, PutBehavior, Send}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by Gábor on 2016.09.28..
  */

case object Subscribed extends Command

case class ReceiveEvent(senderId: ActorId, event: Event) extends Command

object BehaviorId {
  def apply[T: Manifest](): BehaviorId = {
    val m = manifest[T]
    BehaviorId(s"${m.runtimeClass.getSimpleName}_${ActorId.uid}")
  }
}

case class BehaviorId(uid: String = ActorId.uid) extends ActorId

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

  def processEvent(event: Event, context: AppContext): Unit = {
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

  def processCommand(c: Command, behaviorContext: AppContext, behaviorId: BehaviorId): Unit = {
    val commandListener = onCommand(_state)
    if (commandListener.isDefinedAt(c)) {
      commandListener(c).foreach { event =>
        log.info(s"Publishing event: $event")
        behaviorContext.eventStream ! behaviorId -> event
      }
    }
  }

  private[this] var _behaviorContext: Option[(AppContext, BehaviorId)] = None
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
      processCommand(ReceiveEvent(senderId, event), behaviorContext: AppContext, behaviorId)

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand(state: S): PartialFunction[Command, List[Event]]

  def onEvent(state: S, context: AppContext): PartialFunction[Event, S]
}

case class InitBehavior(behaviorContext: AppContext, behaviorId: BehaviorId)


case class PutActor(senderId: ActorId, props: Props)
case class GetBehavior(senderId: ActorId)
case class Send(senderId: ActorId, event: Any)
case class Ask(senderId: ActorId, event: Any)

class ActorStore  extends Actor with ActorLogging {
  var actors = Map.empty[ActorId, ActorRef]

  override def receive: Actor.Receive = {
    case PutActor(senderId, props) =>
      if (actors.contains(senderId)) {
        log.debug(s"$senderId is existing already")
        sender() ! akka.actor.Status.Failure(new IllegalArgumentException(s"The behavior with ID #$senderId is already existing"))
      } else {
        log.debug(s"$senderId is not existing yet")
        val actorRef = context.actorOf(props, s"$senderId")
        //actorRef ! InitBehavior(behaviorContext, senderId)
        context.watch(actorRef)
        actors = actors.updated(senderId, actorRef)
        sender() ! senderId
      }
    case Terminated(terminatedActor) =>
      actors = actors.filterNot { case (senderId, actorRef) => actorRef == terminatedActor }
    case Send(senderId: ActorId, event: Any) =>
      actors.get(senderId).foreach(_ ! event)
    case Ask(senderId: ActorId, event: Any) =>
      actors.get(senderId).map (actor => actor.ask(event)) match {
        case Some(f) => f.pipeTo(sender())
        case None => sender() ! akka.actor.Status.Failure(new IllegalArgumentException(s"Behavior with ID: #$senderId doesn't exist"))
      }
  }

}

class AppContext(actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, implicit val executionContext: ExecutionContext, implicit val timeout: Timeout) {

  val behaviorContext = this

  private[this] val store = actorRefFactory.actorOf(Props(new ActorStore), "ActorStore")

  def create(senderId: ActorId, creator: => Actor): Future[ActorId] = create(senderId, Props(creator))

  def create(senderId: ActorId, props: Props): Future[ActorId] = {
    (store ? PutActor(senderId, props)).collect {
      case b: ActorId => b
    }
  }

  def askBehavior(behaviorId: BehaviorId, event: Any): Future[Any] = {
    store ? Ask(behaviorId, event)
  }

  def sendBehavior(behaviorId: BehaviorId, event: Any): Unit = {
    store ! Send(behaviorId, event)
  }
}
