package eventuate.eventstore

import akka.actor._
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.{MemberEvent, _}
import akka.persistence.{RecoveryCompleted, Recovery, PersistentActor, Persistence}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import eventuate._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.pattern._

/**
 * Created by Gábor on 2016.07.30..
 */

case class SendFailed(error: Throwable)

object ClusterEventStore extends App {
  val ports = List(2551, 2552)
  implicit val resolveTimeout = Timeout(10.seconds)

  val actors = ports.map { port =>
    val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
      withFallback(ConfigFactory.parseString(s"akka.persistence.journal.leveldb.dir=target/journal/$port"))
      .withFallback(ConfigFactory.load())

    // Create an Akka system
    implicit val system = ActorSystem("ClusterSystem", config)
    implicit val cluster = Cluster(system)

    system.actorOf(ClusterEventStoreActor())
  }

  import ExecutionContext.Implicits.global

//    (actors(0) ? TestMessage("22")).onComplete { input =>
//      println(s"1: *********** $input")
//    }
//    (actors(0) ? TestMessage("22")).onComplete { input =>
//      println(s"2: *********** $input")
//    }
//    (actors(1) ? TestMessage("3")).onComplete { input =>
//      println(s"3: *********** $input")
//    }
//    (actors(1) ? TestMessage("4")).onComplete { input =>
//      println(s"4: *********** $input")
//    }
}

object ClusterEventStoreActor {
  def apply()(implicit cluster: Cluster): Props = Props(new ClusterEventStoreActor(cluster))
}

case class EventEnvelope(time: Long, event: Any)

case class SynchronizeRequest(lastEventTime: Long)

case class Add(event: EventEnvelope)

case class Persist(event: EventEnvelope)

case class StartSynchronize(from: Long)

class ClusterEventRestoreActor(reactor: ActorRef, from: Long) extends PersistentActor with ActorLogging {
  override def persistenceId: String = "ClusterEventPersistenceActor"

  override def receiveRecover: Receive = {
    case RecoveryCompleted =>
      reactor ! RecoveryCompleted
      context.stop(self)
    case e: EventEnvelope =>
      if(e.time > from) {
        reactor ! e
      }
    case e =>
  }

  override def receiveCommand: Receive = {
    case e =>
  }
}


class ClusterEventStoreActor(cluster: Cluster) extends PersistentActor with Stash with ActorLogging {
  override def persistenceId: String = "ClusterEventPersistenceActor"

  private[this] var sites: Set[Site] = Set.empty

  private[this] var lastSeqNum: Long = 0L

  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents, classOf[MemberEvent], classOf[LeaderChanged], classOf[UnreachableMember])
  }

  override def postStop(): Unit = cluster.unsubscribe(self)

  case class SiteUp(site: Site)

  case class SiteRemove(site: Site)

  case class Become(isSelfUp: Boolean, isLeader: Boolean, isSynchronized: Boolean, leader: Option[Site])

  def notifySubscribers(e: EventEnvelope): Unit = {
    lastSeqNum = e.time
  }

  override def receiveRecover: Receive = {
    case RecoveryCompleted => log.info("Self recovery is ready")
    case e: EventEnvelope if e.time > lastSeqNum =>
      log.info(s"Self recovering: $e")
      lastSeqNum = e.time
  }


  def mainReceive(isSelfUp: Boolean = false, isLeader: Boolean = false, isSynchronized: Boolean = false, leader: Option[Site] = None): Receive = {

    def canAcceptMessage = isSelfUp && isSynchronized && leader.isDefined

    implicit val ec = context.dispatcher
    implicit val resolveTimeout = Timeout(5.seconds)

    {

      case Become(nIsSelfUp, nIsLeader, nIsSynchronized, nLeader) =>
        log.info(s"Become: canAcceptMessage: ${nIsSelfUp && nIsSynchronized && nLeader.isDefined} [isSelfUp: $nIsSelfUp, isLeader: $nIsLeader, isSynchronized: $nIsSynchronized, leader: ${nLeader.map(_.member.address).getOrElse("None")}]")
        unstashAll()
        context.become(mainReceive(nIsSelfUp, nIsLeader, nIsSynchronized, nLeader))

      /**
       * MemberUp
       */
      case MemberUp(member) =>
        log.info("Member is Up: {}", member.address)
        ClusterUtils.searchActorInMember(self, member).foreach { memberRef =>
          val site = Site(member = member, ref = memberRef, isSelf = false, isLeader = false)
          log.info(s"Site is up: $site")
          sites = sites + site
          if (site.member.address == cluster.selfAddress) {
            self ! Become(true, isLeader, isSynchronized, leader)
          }
        }

      /**
       * UnreachableMember
       */
      case UnreachableMember(member) if member.address != cluster.selfAddress =>
        log.info("Member detected as unreachable: {}", member)
        sites.find(_.member != member).foreach { site =>
          self ! SiteRemove(site)
        }

      /**
       * MemberRemoved
       */
      case MemberRemoved(member, previousStatus) if member.address != cluster.selfAddress =>
        log.info("Member is Removed: {} after {}", member.address, previousStatus)
        sites.find(_.member != member).foreach { site =>
          log.info(s"Site is removed: $site")
          sites = sites - site
          if (site.member.address == cluster.selfAddress) {
            self ! Become(false, isLeader, isSynchronized, leader)
          }
        }

      /**
       * LeaderChanged
       */
      case LeaderChanged(newLeader) if isSelfUp =>
        newLeader match {
          case Some(leaderAddress) =>
            sites.find(_.member.address == leaderAddress).foreach { site =>
              sites = sites.filter(_ != site) + site.copy(isLeader = true)
              if (!newLeader.contains(cluster.selfAddress)) {
                log.info(s"Leader changed, so we need to synchronize")
                self ! Become(isSelfUp, isLeader = false, isSynchronized = false, Some(site))
                site.ref ! StartSynchronize(lastSeqNum)
              } else {
                log.info(s"It became the leader")
                self ! Become(isSelfUp, isLeader = true, isSynchronized = true, Some(site))
              }
            }
          case None =>
            sites = sites.map {
              _.copy(isLeader = false)
            }
            self ! Become(isSelfUp, false, isSynchronized, None)
        }

      case RecoveryCompleted if !isSynchronized =>
        log.info("Synchronization recovery completed")
        self ! Become(isSelfUp, isLeader, isSynchronized = true, leader)

      case e: EventEnvelope if !isSynchronized =>
          log.info(s"Synchronization recovery: $e")
          self ! Add(e)


      case StartSynchronize(from) if isSelfUp && isLeader =>
        val answerTo = sender()
        log.info(s"Leader received a synchronize request from: ${answerTo.path}")
        context.actorOf(Props(new ClusterEventRestoreActor(answerTo, from)))

      /**
       * Event if there is no selfMemberUp or leader
       */
      case event if !canAcceptMessage =>
        log.info(s"Stashing event: $event: $isSelfUp, $isLeader, $isSynchronized")
        stash()

      case memberEvent: ClusterDomainEvent =>

      case Add(event) =>
        log.info(s"Leader sent an event: $event ")

        persist(event) { ee =>
          log.info(s"Node persisted event: $ee")
          notifySubscribers(ee)
        }

      case event if canAcceptMessage =>
        if (isLeader) {
          log.info(s"Leader received message: $event from ${sender()}")
          val lastSender = sender()
          persist(EventEnvelope(lastSeqNum + 1, event)) { ee =>
            log.info(s"Leader persisted event: $ee")
            lastSender ! Sent

            sites.filterNot(_.isLeader).foreach { site =>
              site.ref ! Add(ee)
            }
            notifySubscribers(ee)
          }
        } else {
          log.info(s"Sending message to the leader: $event")
          val lastSender = sender()
          (leader.get.ref ? event) pipeTo lastSender
        }
    }: Receive
  }

  override def receiveCommand: Receive = mainReceive()
}
