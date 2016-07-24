package eventuate

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorRefFactory, ActorSystem, Cancellable, Props, RootActorPath}
import akka.cluster.ClusterEvent._
import akka.cluster.{Cluster, Member}
import akka.pattern._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

/**
  * Created by Gábor on 2016.07.23..
  */
object DistributedLockPlayground extends App {
  val port = 2551
  val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
    withFallback(ConfigFactory.load())

  // Create an Akka system
  implicit val system = ActorSystem("ClusterSystem", config)
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher
  implicit val resolveTimeout = Timeout(5.seconds)

  // Create an actor that handles cluster domain events
  val distributedLock = system.actorOf(Props[DistributedLock], name = "distributedLock")

  distributedLock ! Lock("user", 1).withLock {
    println("Running critical section: 1 ...")
    Thread.sleep(2000)
    println("Finished critical section: 1")
  }

  val lock2 = Lock("user", 1)
  (distributedLock ? lock2).onComplete {
    case Success(granted: LockGranted) =>
      println("Success: 2")
      granted.release()
    case Success(rejection: LockRejected) =>
      println("Rejected: 2")
      lock2.withLock {
        println("Running critical section: 2 ...")
        Thread.sleep(2000)
        println("Finished critical section: 2")
      }
    case Failure(err) => err.printStackTrace()
  }

}

object DistributedLock {
  case class Site(member: Member, ref: ActorRef, uid: String = UUID.randomUUID().toString)

  case class Clock(counter: Long) {
    val currentTime = System.currentTimeMillis()

    def next: Clock = Clock(counter + (System.currentTimeMillis() - currentTime))
  }

  case class LockConfirmations(confirmedSites: Map[Site, Boolean], sender: ActorRef) {
    lazy val sites = confirmedSites.keySet

    def confirm(site: Site): Either[LockConfirmations, ActorRef] = {
      val newConfirmedSites = confirmedSites.updated(site, true)

      if(newConfirmedSites.exists(!_._2)) {
        Left(copy(confirmedSites = newConfirmedSites))
      } else {
        Right(sender)
      }
    }
  }

  object LockConfirmations {
    def apply(sites: Set[Site], sender: ActorRef): LockConfirmations = {
      new LockConfirmations(confirmedSites = sites.map(_ -> false).toMap, sender = sender)
    }
  }

}

case class Lock(aggregate: String, id: Long)(implicit val factory: ActorRefFactory) {
  private[this] val p = Promise[LockGranted]
  private[eventuate] val releaseListener = factory.actorOf(Props(new Actor {
    override def receive: Receive = {
      case lg@LockGranted(locker, aggregateId, uid) =>
        p.success(lg)
      case _ =>
        p.failure(new Exception("Unknown message"))
    }
  }))

  private[this] val future: Future[LockGranted] = p.future
  implicit val ec = factory.dispatcher
  def withLock(f: => Unit): Lock = {
    future.foreach{ lock =>
      f
      lock.release()
    }
    this
  }
}
case class LockRequest(sender: ActorRef, releaseListener: ActorRef, uid: String = UUID.randomUUID().toString)
case class LockReleaseRequest(aggregateId: String, uid: String)

case class LockGranted(locker: ActorRef, aggregateId: String, uid: String) {
  def release(): Unit = locker ! LockReleaseRequest(aggregateId, uid)
}
case class LockRejected(locker: ActorRef, aggregateId: String, uid: String)

class DistributedLock extends Actor with ActorLogging {
  import DistributedLock._

  private[this] val cluster = Cluster(context.system)
  private[this] var sites = Set.empty[Site]
  private[this] implicit var clock: Clock = Clock(0)
  private[this] implicit val ec = context.dispatcher
  private[this] var tickTask: Cancellable = _
  private[this] var confirmations: Map[String, LockConfirmations] = Map.empty

  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents, classOf[MemberEvent], classOf[UnreachableMember])
  }

  override def postStop(): Unit = cluster.unsubscribe(self)

  /**
    * Return an actorRef for a member's same actor
    *
    * @param member
    * @return
    */
  private[this] def actorRefForMember(member: Member): Future[ActorRef] = {
    val pathWithoutAddress = self.path.toStringWithoutAddress.split("/")
    val newActorPath = pathWithoutAddress.foldLeft(RootActorPath(member.address): ActorPath) { (path, part) =>
      path / part
    }
    implicit val resolveTimeout = Timeout(5.seconds)
    context.actorSelection(newActorPath).resolveOne()
  }

  /**
    *
    * @param event
    * @param eventSender
    */
  private[this] def sendEventToSites(event: Any, eventSender: ActorRef): Unit = {
    clock = clock.next
    val uid = UUID.randomUUID().toString
    val eventConfirmations = event match {
      case Tick =>
        LockConfirmations(sites, context.actorOf(Props(new Actor {
          override def receive: Receive = {
            case Sent =>
          }
        })))
      case _ =>
        LockConfirmations(sites, eventSender)
    }
    confirmations = confirmations.updated(uid, eventConfirmations)

    log.debug(s"Sending: $event to ${eventConfirmations.sites}")
    eventConfirmations.sites.foreach { site =>
      val stampedEvent = StampedEvent(clock.counter, event, uid, site.uid)
      site.ref ! stampedEvent
    }
  }

  private[this] def synchronizeClock(timestamp: Long): Unit = {
    clock = clock.next
    if (timestamp > clock.counter) {
      clock = Clock(timestamp + 1)
      log.debug(s"Modifying clock to $clock")
    } else {
      log.debug(s"Clock is $clock")
    }

  }

  def isLeader: Boolean = {
    cluster.state.leader.exists { leader =>
      sites.exists(_.member.address == leader)
    }
  }

  def leaderRef: Option[ActorRef] = {
    cluster.state.leader.flatMap { leader =>
      sites.find(_.member.address == leader)
    }.map(_.ref)
  }

  var locks = Map.empty[String, Vector[LockRequest]]
  override def receive = {

    /**
      * Acquire lock
      */
    case lock@Lock(aggregate, id) if isLeader =>
      println("this is the leader")
      val aggregateId = s"$aggregate-$id"
      val lockRequest = LockRequest(sender(), lock.releaseListener)
      locks = locks.updated(aggregateId, locks.getOrElse(aggregateId, Vector.empty) :+  lockRequest)

      if(locks(aggregateId).head == lockRequest) {
        val lg = LockGranted(self, aggregateId, lockRequest.uid)
        lockRequest.releaseListener ! lg
        lockRequest.sender ! lg
      } else {
        lockRequest.sender ! LockRejected(self, aggregateId, lockRequest.uid)
      }

    case lock: Lock if !isLeader =>
      println("this is not the leader")
      leaderRef.foreach { leader =>
        leader ! lock
      }
    /**
      * Release a lock
      */
    case LockReleaseRequest(aggregateId, uid) =>
      val o = locks(aggregateId)
      locks =  locks.updated(aggregateId, o.filter(_.uid != uid))
      if(locks(aggregateId).isEmpty) {
        locks = locks - aggregateId
      }

      locks.get(aggregateId)
        .foreach(_.headOption
          .foreach(lockRequest => lockRequest.releaseListener ! LockGranted(self, aggregateId, lockRequest.uid)))

    // Got a message from other node's ClusterBroadcast actor
    case StampedEvent(departedTime, event, uid, site) =>
      synchronizeClock(departedTime)
      sender() ! ConfirmStampedEvent(uid, site, clock.counter)
      event match {
        case Tick =>
        //Tick is only for synchronization
      }

    //Confirmation from other node's ClusterBroadcast actor for an event
    case ConfirmStampedEvent(uid, siteUid, receivedTime) =>
      synchronizeClock(receivedTime)
      sites.find(_.uid == siteUid).foreach { site =>
        log.info(s"Confirming $uid from $siteUid")
        confirmations(uid).confirm(site) match {
          case Left(newConfirmations) =>
            log.debug("Still need more confirm")
            confirmations = confirmations.updated(uid, newConfirmations)
          case Right(sender) =>
            log.debug("All site confirmed")
            confirmations = confirmations - uid
            sender ! Sent
        }
      }

    case MemberUp(member) if member.address != cluster.selfAddress =>
      log.debug("Member is Up: {}", member.address)
      actorRefForMember(member).foreach { memberRef =>
        sites = sites + Site(member, memberRef)
      }
    case UnreachableMember(member) if member.address != cluster.selfAddress =>
      log.debug("Member detected as unreachable: {}", member)
      sites = sites.collect {
        case e@Site(m, _, _) if member != m => e
      }
    case MemberRemoved(member, previousStatus) if member.address != cluster.selfAddress =>
      log.debug("Member is Removed: {} after {}", member.address, previousStatus)
      sites = sites.collect {
        case e@Site(m, _, _) if member != m => e
      }
    case MemberUp(member) if member.address == cluster.selfAddress =>
      tickTask = context.system.scheduler.schedule(5.seconds, 5.seconds, self, Tick)
      log.info(s"It joined to cluster: ${cluster.state.leader} / ${cluster.selfAddress}")

    case _: MemberEvent => // ignore

  }

}
