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
//  (actors(0) ? TestMessage("1")).onComplete { input =>
//    println(s"1: *********** $input")
//  }
//  (actors(0) ? TestMessage("2")).onComplete { input =>
//    println(s"2: *********** $input")
//  }
//  (actors(1) ? TestMessage("3")).onComplete { input =>
//    println(s"3: *********** $input")
//  }
//  (actors(1) ? TestMessage("4")).onComplete { input =>
//    println(s"4: *********** $input")
//  }
}

object ClusterEventStoreActor {
  def apply()(implicit cluster: Cluster): Props = Props(new ClusterEventStoreActor(cluster))
}

case class EventEnvelope(time: Long, event: Any)
case class SynchronizeRequest(lastEventTime: Long)

case class Add(event: Any, sender: Option[ActorRef])

case class Persist(event: EventEnvelope)

case object StartSynchronize

class ClusterEventPersistenceActor extends PersistentActor with ActorLogging  {
  override def persistenceId: String = "ClusterEventPersistenceActor"
  override def recovery = Recovery.none
  
  override def receiveRecover: Receive = {
    case e => 
  }

  override def receiveCommand: Receive = {
    case Persist(eventEnvelope) => persist(eventEnvelope) { event =>
      log.info(s"Persisted: $event")
      sender() ! event
    }
    case e => 
  }
}

class ClusterEventRestoreActor(reactor: ActorRef) extends PersistentActor with ActorLogging  {
  override def persistenceId: String = "ClusterEventPersistenceActor"

  override def receiveRecover: Receive = {
    case RecoveryCompleted =>
      reactor ! RecoveryCompleted
      context.stop(self)
    case e: EventEnvelope =>
      reactor ! e
    case e =>
  }

  override def receiveCommand: Receive = {
    case e =>
  }
}


class ClusterEventStoreActor(cluster: Cluster) extends Actor with Stash with ActorLogging {

  private[this] var sites: Set[Site] = Set.empty
  private[this] val persistenceActor = context.actorOf(Props(new ClusterEventPersistenceActor), "PersistenceActor")

  private[this] var lastSeqNum: Long = 0 //events.lastOption.map(_.time).getOrElse(0)
  
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
          if(site.member.address == cluster.selfAddress) {
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
          if(site.member.address == cluster.selfAddress) {
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
              if(!newLeader.contains(cluster.selfAddress)) {
                log.info(s"Leader changed, so we need to synchronize")
                self ! Become(isSelfUp, isLeader = false, isSynchronized = false, Some(site))
                println(s"Sending StartSynchronize from ${self}")
                site.ref ! StartSynchronize
              } else {
                log.info(s"It became the leader")
                if(!isSynchronized) {
                  log.info(s"It's not synchronized, it starts to")
                  context.actorOf(Props(new ClusterEventRestoreActor(self)))
                }
                self ! Become(isSelfUp, isLeader = true, isSynchronized, Some(site))
              }
            }
          case None =>
            sites = sites.map {_.copy(isLeader = false)}
            self ! Become(isSelfUp, false, isSynchronized, None)
        }

      case RecoveryCompleted if !isSynchronized =>
        log.info("Recovery completed")
        self ! Become(isSelfUp, isLeader, isSynchronized = true, leader)
        
      case e: EventEnvelope if !isSynchronized && e.time > lastSeqNum =>
        log.info(s"Event recovered: $e")
//        events = events :+ e
        notifySubscribers(e)
        

      case StartSynchronize if isSelfUp && isLeader =>
        val answerTo = sender()
        log.info(s"Leader received a synchronize request from: ${answerTo.path}")
        context.actorOf(Props(new ClusterEventRestoreActor(answerTo)))        
        
      /**
        * Event if there is no selfMemberUp or leader
        */
      case event if !canAcceptMessage =>
        log.info(s"Stashing event: $event: $isSelfUp, $isLeader, $isSynchronized")
        stash()

      case memberEvent: ClusterDomainEvent =>

      case Add(event, sender) =>
        log.info(s"Added to local event store: $event ")
        
//        val lastTime = events.lastOption.map(_.time).getOrElse(0L)
        
        val ee = EventEnvelope(lastSeqNum + 1, event)
        notifySubscribers(ee)
//        events =  events :+ ee
        
        val lastSender = this.sender()
        (persistenceActor ? Persist(ee)).foreach { _ =>
          if (isLeader) {
            log.info(s"Leader sends out to nodes: $event")
            val f = sites.filterNot(_.isLeader).map { site =>
              site.ref ? Add(event, None)
            }
            Future.sequence(f).onComplete {
              case Failure(error) =>
                log.warning("Leader couldn't send out message to all nodes")
                sender.getOrElse(lastSender) ! SendFailed(new Exception("Couldn't send to all node"))
              case Success(e) =>
                log.warning(s"Leader got acknowledge from all the nodes for $event")
                sender.getOrElse(lastSender) ! Sent
            }
          } else {
            log.info(s"Node sends back acknowledge to leader for $event")
            sender.getOrElse(lastSender) ! Sent
          }
        }

      case event if canAcceptMessage =>
        if(isLeader) {
          log.info(s"Leader received message: $event")
          self ? Add(event, Some(this.sender()))
        } else {
          log.info(s"Sending message to the leader: $event")
          (leader.get.ref ? event) pipeTo sender()
        }
    }: Receive
  }

  override def receive: Receive = mainReceive()
}
