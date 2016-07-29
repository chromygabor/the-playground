package eventuate

import java.util.UUID

import akka.actor._
import akka.cluster.ClusterEvent._
import akka.cluster.{Cluster, Member}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
 * Created by Gábor on 2016.07.23..
 */


object DistributedLockPlayground extends App {
  val port = 2551
  val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
    withFallback(ConfigFactory.load())

  // Create an Akka system
  implicit val system = ActorSystem("ClusterSystem", config)
  val cluster = Cluster(system)
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher
  implicit val resolveTimeout = Timeout(5.seconds)

  // Create an actor that handles cluster domain events
//  val distributedLock = system.actorOf(Props(new DistributedLockActor(cluster)), name = "distributedLock")
//  distributedLock ! TestMessage("foo")
//
//  distributedLock ! LockMessage("user-1").withLock {
//    println("Running critical section: 1 ...")
//    Thread.sleep(2000)
//    println("Finished critical section: 1")
//  }

  val lr1 = DistributedLock(cluster, "user-1")
//  val lr2 = DistributedLock(cluster, "user-2")
//  
//  for {
//    l1 <- lr1.waitFor
//    l2 <- lr2.waitFor
//  } yield {
//    println("it acquired all the locks")
//    l1.release()
//    l2.release()
//  }
//  
//  lr1.foreach {
//    case e: Available => println("lr1 is available")
//    case e: Unavailable => println("lr1 is unavailable")
//  }
//  
  
  //val f = distributedLock ! LockMessage("user-1")
  
  
  //
  //  val lock2 = Lock("user", 1)
  //  (distributedLock ? lock2).onComplete {
  //    case Success(granted: LockGranted) =>
  //      println("Success: 2")
  //      granted.release()
  //    case Success(rejection: LockRejected) =>
  //      println("Rejected: 2")
  //      lock2.withLock {
  //        println("Running critical section: 2 ...")
  //        Thread.sleep(2000)
  //        println("Finished critical section: 2")
  //      }
  //    case Failure(err) => err.printStackTrace()
  //  }

}

trait LockAvailability
case class Available() extends LockAvailability {
  def release(): Unit = ???
}
case class Unavailable() extends LockAvailability
object DistributedLock {
  
  private[this] var clusters = Map.empty[Cluster, ActorRef]

  def apply(cluster: Cluster, aggregateId: String)(implicit actorRefFactory: ActorRefFactory): DistributedLock = {
    val lm = LockMessage(aggregateId)
    val dl = new DistributedLock(cluster, s"${aggregateId}")
    getOrCreate(cluster) !(lm, dl.senderActor)
    dl
  }
  
  private[this] def getOrCreate(cluster: Cluster)(implicit actorRefFactory: ActorRefFactory): ActorRef = {
    clusters.get(cluster) match {
      case Some(ref) => ref
      case None =>
        val newClusterLock = actorRefFactory.actorOf(Props(new DistributedLockActor(cluster)), s"DistributedLock-Cluster")
        clusters = clusters + (cluster -> newClusterLock)
        newClusterLock ! TestMessage("10")
        newClusterLock
    }
  }
  
}

/**
 * Distributed lock
 * @param cluster
 * @param lockId
 */
class DistributedLock private (cluster: Cluster, lockId: String)(implicit val actorRefFactory: ActorRefFactory) {
  private[this] val immediateLockResponse = Promise[LockAvailability]
  private[this] val lockAvailable = Promise[Available]

  val senderActor = actorRefFactory.actorOf(Props(new Actor with ActorLogging {
    override def receive: Actor.Receive = {
      case LockGranted => 
        log.debug(s"LockGranted $lockId")
        val available = Available()
        if(!immediateLockResponse.isCompleted) {
          log.debug(s"immediateResponse is not completed yet for $lockId")
          immediateLockResponse.success(available)
        }
        log.debug(s"Complete lockAvailable for $lockId")
        lockAvailable.success(available)
      case LockRejected =>
        log.debug(s"LockRejected for $lockId")
        log.debug(s"Complete immediateResponse for $lockId")
        immediateLockResponse.success(Unavailable())
        
      case e =>
        log.error(s"Unknown message type received: $e for $lockId")
        immediateLockResponse.failure(new IllegalStateException(s"Unknown message type received: $e"))
    }
  }))
  
  def waitFor: Future[Available] = lockAvailable.future
  def foreach(f: PartialFunction[LockAvailability, Unit])(implicit ec: ExecutionContext) = immediateLockResponse.future.foreach(f)
  
}

/** ******************************************************
  * DistributedLock
  * *******************************************************/
object DistributedLockActor {
  
  case class Site(member: Member, ref: ActorRef, isSelf: Boolean, isLeader: Boolean, uid: String = UUID.randomUUID().toString)

  case class Clock(counter: Long) {
    val currentTime = System.currentTimeMillis()

    def next: Clock = Clock(counter + (System.currentTimeMillis() - currentTime))
  }

  case class LockConfirmations(confirmedSites: Map[Site, Boolean], sender: ActorRef) {
    lazy val sites = confirmedSites.keySet

    def confirm(site: Site): Either[LockConfirmations, ActorRef] = {
      val newConfirmedSites = confirmedSites.updated(site, true)

      if (newConfirmedSites.exists(!_._2)) {
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

case class LockMessage(aggregateId: String)
case class LockReleaseRequest(aggregateId: String, uid: String)

case object LockGranted
case object LockRejected 

/** ************************************************
  * DistributedLock
  */
class DistributedLockActor(cluster: Cluster) extends Actor with Stash with ActorLogging {
  import DistributedLockActor._

  case class RequestedLock(sender: ActorRef, uid: String = UUID.randomUUID().toString)

  private[this] var sites = Set.empty[Site]
  private[this] implicit var clock: Clock = Clock(0)
  private[this] implicit val ec = context.dispatcher
  private[this] var tickTask: Cancellable = _
  private[this] var confirmations: Map[String, LockConfirmations] = Map.empty

  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents, classOf[MemberEvent], classOf[LeaderChanged], classOf[UnreachableMember])
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


  var locks = Map.empty[String, Vector[RequestedLock]]

  case class Become(sites: Set[Site], isSelfMemberUp: Boolean, leader: Option[Site])
  
  def myReceive(sites: Set[Site], isSelfMemberUp: Boolean, leader: Option[Site]): Receive = {
    /**
     * Become
     */
    case become@Become(sites, isSelfMemberUp, leader) =>
      log.debug(s"Becoming: $become")
      unstashAll()
      context.become(myReceive(sites, isSelfMemberUp, leader))

    /**
     * MemberUp
     */
    case MemberUp(member) =>
      log.debug("Member is Up: {}", member.address)
      actorRefForMember(member).foreach { memberRef =>
        val newSites = sites + Site(member = member, ref = memberRef, isSelf = false, isLeader = false)
        val isOurMemberUp = member.address == cluster.selfAddress
        self ! Become(sites = newSites, isSelfMemberUp = isOurMemberUp, leader = leader)
      }

    /**
     * UnreachableMember
     */
    case UnreachableMember(member) if member.address != cluster.selfAddress =>
      log.debug("Member detected as unreachable: {}", member)
      val newSites = sites.filterNot(_.member != member)
      val isOurMemberUp = member.address != cluster.selfAddress
      self ! Become(sites = newSites, isSelfMemberUp = isOurMemberUp, leader = leader)

    /**
     * MemberRemoved
     */
    case MemberRemoved(member, previousStatus) if member.address != cluster.selfAddress =>
      log.debug("Member is Removed: {} after {}", member.address, previousStatus)
      val newSites = sites.filterNot(_.member != member)
      val isOurMemberUp = member.address != cluster.selfAddress
      self ! Become(sites = newSites, isSelfMemberUp = isOurMemberUp, leader = leader)

    /**
     * LeaderChanged
     */
    case LeaderChanged(Some(leaderAddress)) if isSelfMemberUp =>
      //      tickTask = context.system.scheduler.schedule(5.seconds, 5.seconds, self, Tick)

      sites.find(_.member.address == leaderAddress).foreach { site =>
        log.debug(s"Leader changed to: $site")
        val newSites = sites.filterNot(_ == site) + site.copy(isLeader = true)
        self ! Become(sites = newSites, isSelfMemberUp = isSelfMemberUp, leader = Some(site))
      }

    /**
     * LeaderChanged
     */
    case LeaderChanged(None) if isSelfMemberUp =>
      log.debug(s"There is no leader")
      sites.find(_.isLeader).foreach { site =>
        val newSites = sites.filterNot(_ == site) + site.copy(isLeader = false)
        self ! Become(sites = newSites, isSelfMemberUp = isSelfMemberUp, leader = Some(site))
      }

    /**
     * Event if there is no selfMemberUp or leader
     */
    case event if !isSelfMemberUp || leader.isEmpty =>
      log.debug(s"Stashing event: $event")
      stash()

    /**
     * Lock if leader is self
     */
    case lock@LockMessage(aggregateId) if leader.exists(_.member.address == cluster.selfAddress) =>
      log.debug(s"Lock requested for $aggregateId")
      val lockRequest = RequestedLock(sender())
      locks = locks.updated(aggregateId, locks.getOrElse(aggregateId, Vector.empty) :+ lockRequest)

      if (locks(aggregateId).head == lockRequest) {
        lockRequest.sender ! LockGranted
      } else {
        lockRequest.sender ! LockRejected
      }

    /**
     * Forwarding lock to the leader
     */
    case lock: LockMessage =>
      log.debug(s"Forwarding $lock to leader")
      leader.foreach{ leaderSite => 
        leaderSite.ref ! lock
      }

    /**
     * Release a lock
     */
    case LockReleaseRequest(aggregateId, uid) if leader.exists(_.member.address == cluster.selfAddress) =>
      log.debug(s"LockRelease requested for $aggregateId")
      locks = locks.updated(aggregateId, locks(aggregateId).filter(_.uid != uid))
      if (locks(aggregateId).isEmpty) {
        locks = locks - aggregateId
      }

      locks.get(aggregateId)
        .foreach(_.headOption
          .foreach(lockRequest => lockRequest.sender ! LockGranted))

    /**
     * Forwarding lockRelease to the leader
     */
    case lockRelease: LockReleaseRequest if !leader.exists(_.member.address == cluster.selfAddress) =>
      log.debug(s"Forwarding $lockRelease to leader")
      leader.foreach{ leaderSite =>
        leaderSite.ref ! lockRelease
      }

    /**
     * TestMessage
     */
    case TestMessage(message) =>
      val isLeader = leader.exists(_.member.address == cluster.selfAddress)
      println(s"Message has received: $message, $isLeader")
  }
  
  /**
   * No leader function
   */
  def noLeader: Receive = {
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

    case event =>
      println(s"stashing: $event")
      stash()

  }

  /**
   *
   * @return
   */
  override val receive = myReceive(Set(), false, None)

}
