package ui

import java.util.UUID

import akka.actor._
import akka.pattern._
import akka.util.Timeout

import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * Created by Gábor on 2016.09.28..
  */

case class Put(serviceId: ServiceId, service: AnyRef)

case class Get(actorId: ServiceId)

//case class Send(actorId: ServiceId, event: Any)
//case class Ask(actorId: ServiceId, event: Any)

class ServiceStore(implicit val ec: ExecutionContext, val timeout: Timeout) extends Actor with ActorLogging {
  var services = Map.empty[ServiceId, AnyRef]

  override def receive: Actor.Receive = {
    case Put(serviceId, service) =>
      if (services.contains(serviceId)) {
        log.debug(s"$serviceId is existing already")
        sender() ! akka.actor.Status.Failure(new IllegalArgumentException(s"The behavior with ID #$serviceId is already existing"))
      } else {
        log.debug(s"$serviceId is not existing yet, it creates a new entry for it")
        if (service.isInstanceOf[ActorRef]) {
          context.watch(service.asInstanceOf[ActorRef])
        }
        services = services.updated(serviceId, service)
        sender() ! serviceId
      }
    case Get(serviceId) =>
      if (services.contains(serviceId)) {
        val service = services(serviceId)
        log.debug(s"$serviceId is existing it can return: $service")
        sender() ! service
      } else {
        log.debug(s"$serviceId is not existing yet it sends back an error")
        sender() ! akka.actor.Status.Failure(new IllegalArgumentException(s"The behavior with ID #$serviceId is not existing int the store"))
      }
    case Terminated(terminatedActor) =>
      services = services.filterNot { case (actorId, actorRef) => actorRef == terminatedActor }

    //    case Send(actorId: ServiceId, event: Any) =>
    //      actors.get(actorId).foreach(_ ! event)
    //    case Ask(actorId: ServiceId, event: Any) =>
    //      actors.get(actorId).map (actor => actor.ask(event)) match {
    //        case Some(f) => f.pipeTo(sender())
    //        case None => sender() ! akka.actor.Status.Failure(new IllegalArgumentException(s"Behavior with ID: #$actorId doesn't exist"))
    //      }
  }

}

/**
  * AppContext
  *
  * @param actorRefFactory
  * @param owner
  * @param eventStream
  * @param executionContext
  * @param timeout
  */
class AppContext(val actorRefFactory: ActorRefFactory, owner: ActorRef, val eventStream: ActorRef, implicit val executionContext: ExecutionContext, implicit val timeout: Timeout) {

  val behaviorContext = this

  private[this] val store = actorRefFactory.actorOf(Props(new ServiceStore), "ActorStore")

  def replay(f: Envelope => Boolean, actorRef: ActorRef): Unit = {
    eventStream ! Replay(f, actorRef)
  }

  def create(serviceId: ServiceId, service: AnyRef): Future[ServiceId] = {
    val storableService = service match {
      case props: Props =>
        actorRefFactory.actorOf(props)
      case e => e
    }
    (store ? Put(serviceId, storableService)).collect {
      case b: ServiceId => b
    }
  }

  def subscribe(subscriber: ActorRef) = eventStream ! Subscribe(subscriber)

  def publish(event: Any): Unit = eventStream ! event

  def get(actorId: ServiceId): Future[AnyRef] = {
    (store ? Get(actorId)).collect {
      case a: AnyRef => a
    }
  }
}


class BehaviorContext(appContext: AppContext) {
  def actorRefFactory: ActorRefFactory = appContext.actorRefFactory

  implicit def backgroundExecutor: ExecutionContext = appContext.executionContext

  def timeout: Timeout = appContext.timeout

  private def createBehavior(clazz: Class[_]): Future[Props] = {
    import scala.reflect.runtime.{universe => ru}

    implicit val mirror = ru.runtimeMirror(this.getClass.getClassLoader)

    def getType(clazz: Class[_])(implicit runtimeMirror: ru.Mirror) = runtimeMirror.classSymbol(clazz).toType


//    val cla = getType(clazz).typeSymbol.asClass
    //val cm = mirror.reflectClass(cla)
    val constructor = getType(clazz).decl(ru.termNames.CONSTRUCTOR).asMethod
    //val constructorMethod = cm.reflectConstructor(constructor)
    val args = constructor.asMethod.paramLists.head map { p => p.name.decodedName.toString -> p.typeSignature }

    val resolvedArgs = args.map {
      case (dependencyName, tpe) => appContext.get(ServiceId(dependencyName.capitalize))
    }

    Future.sequence(resolvedArgs).map { resolvedArgs =>
      Props(clazz, resolvedArgs: _*)
    }
  }


  def create[B <: Behavior[_] : Manifest](): Future[BehaviorId] = create(BehaviorId(ServiceId.singleton.uid))

  def create[B <: Behavior[_] : Manifest](behaviorId: BehaviorId): Future[BehaviorId] = {
    val m = manifest[B]
    createBehavior(m.runtimeClass).flatMap(create(behaviorId, _))
  }

  //def create(behaviorId: BehaviorId, creator: => Actor): Future[BehaviorId] = create(behaviorId, Props(creator))

  def create(behaviorId: BehaviorId, props: Props): Future[BehaviorId] = {
    val p = Promise[BehaviorId]
    val behaviorRef = BehaviorRef(actorRefFactory.actorOf(props, s"ActorOf-${behaviorId.uid}"))
    appContext.create(behaviorId, behaviorRef).collect {
      case b: BehaviorId => b
    }.foreach { _ =>
      behaviorRef ! InitBehavior(this, behaviorId)
      p.success(behaviorId)
    }
    p.future
  }

  def sendCommandToBehavior(behaviorId: BehaviorId, cmd: Command): Unit = {
    appContext.get(behaviorId).collect {
      case behavior: BehaviorRef => behavior
    }.foreach(_ ! cmd)
  }

  def subscribe(subscriber: ActorRef) = appContext.subscribe(subscriber)

  def publish(event: (BehaviorId, Event)): Unit = appContext.publish(event)

  def behaviorOf(behaviorId: BehaviorId): Future[BehaviorRef] = appContext.get(behaviorId).collect {
    case ref: BehaviorRef => ref
  }

  def replay(f: Envelope => Boolean, actorRef: ActorRef): Unit = appContext.replay(f, actorRef)

}

case class ReceiveEvent(actorId: ServiceId, event: Event) extends Command

object BehaviorId {
  def apply[T: Manifest]: BehaviorId = {
    val m = manifest[T]
    BehaviorId(s"${m.runtimeClass.getSimpleName}_${ServiceId.uid}")
  }
}

case class BehaviorId(override val uid: String = ServiceId.uid) extends ServiceId(uid)

case class BehaviorRef(actorRef: ActorRef) {
  def !(msg: Any) = actorRef ! msg
}

object Completable {
  def apply[S, T](onSuccess: (S, T) => List[Event]): Fallable[S] = ???
}

trait Fallable[S] {
  def onFailure(onFailure: (S, Throwable) => List[Event]): Unit
}


trait MyTask[S, V] extends SimpleEvent {
  def id: TaskId
  def onSuccess(onComplete: (S, V) => List[Event]): this.type
  def onFailure(onComplete: (S, Throwable) => List[Event]): this.type

  override def toString(): String = {
    s"TaskStarted($id)"
  }
}

trait TaskId
case class SimpleTaskId(uid: String =  UUID.randomUUID().toString) extends TaskId
case class QueueTaskId(uid: String =  UUID.randomUUID().toString) extends TaskId

object MyTask {

  object TaskStarted {
    def unapply(e: Event): Option[TaskId] = e match {
      case t: MyTask[_, _] =>
        Some(t.id)
      case _ => None
    }
  }
  case class TaskFinished(id: TaskId) extends SimpleEvent
}


trait Behavior[S] extends Actor with ActorLogging with Stash {

  private var _state: S = _
  private var _initialized = false

  private[this] var _context: Option[(BehaviorContext, BehaviorId)] = None

  def behaviorContext = _context.map(_._1).getOrElse(throw new IllegalAccessException("BehaviorContext isn't set yet"))

  def behaviorId = _context.map(_._2).getOrElse(throw new IllegalAccessException("BehaviorContext isn't set yet"))

  implicit lazy val backgroundExecutor: ExecutionContext = _context.map(_._1.backgroundExecutor).getOrElse(throw new IllegalAccessException("BehaviorContext isn't set yet"))
  implicit lazy val timeout: Timeout = _context.map(_._1.timeout).getOrElse(throw new IllegalAccessException("BehaviorContext isn't set yet"))

  def initialState: S

  implicit def commandToList(cmd: Command): Seq[Event] = {
    val commandHandler = onCommand(_state)
    if (commandHandler.isDefinedAt(cmd)) commandHandler(cmd)
    else Nil
  }

  implicit def eventToListOfEvent(evt: Event): List[Event] = evt :: Nil

  case class Deffer[T](f: (S, T) => List[Event], result: T)

  private var tasks = Map[TaskId, String]()

  def taskAs[V: Manifest](iId: TaskId, future: => Future[Any]): MyTask[S, V] = {
    val f = future.collect {
      case v: V => v
    }
    task(iId, f)
  }

  def task[V](iId: TaskId, future: => Future[V]): MyTask[S, V] = {
    val t = new MyTask[S, V] {
      val requestId = UUID.randomUUID().toString

      tasks = tasks.updated(iId, requestId)
      val id = iId
      val p = Promise[V]

      future.onComplete(p.complete)

      def onSuccess(onComplete: (S, V) => List[Event]): this.type = {
        p.future.onSuccess {
          case v =>
            id match {
              case SimpleTaskId(_) if tasks(id) == requestId =>
                val pf = { (state: S, value: V) => MyTask.TaskFinished(id) :: onComplete(state, value) }
                self ! Deffer(pf, v)
                tasks = tasks - id
              case SimpleTaskId(_) =>
              case QueueTaskId(_) =>
                val pf = { (state: S, value: V) => MyTask.TaskFinished(id) :: onComplete(state, value) }
                self ! Deffer(pf, v)
                tasks = tasks - id
            }
        }
        this
      }

      def onFailure(onComplete: (S, Throwable) => List[Event]): this.type = {
        p.future.onFailure {
          case error =>
            val pf = { (state: S, value: Throwable) => MyTask.TaskFinished(id) :: onComplete(state, value) }
            self ! Deffer(pf, error)
        }
        this
      }
    }
    t
  }


//  def onSuccessAs[T: Manifest](future: Future[Any])(onSuccess: (S, T) => List[Event]): Fallable = {
//    val p = Promise[T]
//    future.collect {
//      case v: T => v
//    }.onComplete {
//      case Success(v) => self ! Deffer(onSuccess, v)
//      case Failure(t) => p.failure(t)
//    }
//    new Fallable {
//      override def onFailure(onFailure: (S, Throwable) => List[Event]) = {
//        p.future.onFailure {
//          case t => self ! Deffer(onFailure, t)
//        }
//      }
//    }
//  }
//
//  def onSuccess[T](id: String, future: Future[T])(onSuccess: (S, T) => List[Event]): Fallable = {
//    val p = Promise[T]
//    future.onComplete {
//      case Success(v) => self ! Deffer(onSuccess, v)
//      case Failure(t) => p.failure(t)
//    }
//    new Fallable {
//      override def onFailure(onFailure: (S, Throwable) => List[Event]) = {
//        p.future.onFailure {
//          case t => self ! Deffer(onFailure, t)
//        }
//      }
//    }
//  }

  //  def onSuccess[T](f: => T)(onComplete: (S, T) => List[Event]): Unit = {
  //    onSuccess(Future(f))(onComplete)
  //  }

  def processEvent(event: Event, context: BehaviorContext): Unit = {
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

  def processCommand(c: Command, behaviorContext: BehaviorContext, behaviorId: BehaviorId): Unit = {
    val commandListener = onCommand(_state)
    if (commandListener.isDefinedAt(c)) {
      commandListener(c).foreach { event =>
        log.info(s"Publishing event: $event")
        behaviorContext.publish(behaviorId -> event)
      }
    }
  }

  val receive: Receive = {
    case InitBehavior(behaviorContext, behaviorId) =>
      behaviorContext.subscribe(self)

      unstashAll()
      _state = initialState

      _context = Some((behaviorContext, behaviorId))
      if (!_initialized) self ! Init

    case e if _context.isEmpty =>
      log.debug(s"Stashing $e")
      stash()
    case Deffer(f, result) =>
      f(_state, result).foreach { event =>
        behaviorContext.publish(behaviorId -> event)
      }

    case c: Command =>
      log.debug(s"Processing command: $c")
      processCommand(c, behaviorContext, behaviorId)

    case Envelope(actorId, event, _) if actorId == behaviorId =>
      processEvent(event, behaviorContext)

    case Envelope(actorId, event, _) =>
      processCommand(ReceiveEvent(actorId, event), behaviorContext: BehaviorContext, behaviorId)

    case m => log.warning(s"Behavior can only accept command or event. Message received is: $m")
  }

  def onCommand(state: S): PartialFunction[Command, Seq[Event]]

  def onEvent(state: S, context: BehaviorContext): PartialFunction[Event, S]
}

case class InitBehavior(behaviorContext: BehaviorContext, behaviorId: BehaviorId)

