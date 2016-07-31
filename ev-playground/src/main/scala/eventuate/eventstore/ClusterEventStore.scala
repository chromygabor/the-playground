package eventuate.eventstore

import akka.actor.{Actor, ActorLogging, ActorSystem, Address, Props, Stash}
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.{MemberEvent, _}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import eventuate.{ClusterUtils, Site, TestMessage}

import scala.concurrent.duration._

/**
  * Created by Gábor on 2016.07.30..
  */


object ClusterEventStore extends App {
  val ports = List(2551)

  ports.foreach { port =>
    val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
      withFallback(ConfigFactory.load())

    // Create an Akka system
    implicit val system = ActorSystem("ClusterSystem", config)
    implicit val cluster = Cluster(system)
    implicit val ec = system.dispatcher
    implicit val resolveTimeout = Timeout(5.seconds)

    val s = system.actorOf(ClusterEventStoreActor())

    s ! TestMessage(s"$port")
  }

}

object ClusterEventStoreActor {
  def apply()(implicit cluster: Cluster): Props = Props(new ClusterEventStoreActor(cluster))
}

case class EventEnvelope(time: Long, event: Any)
case class Synchronize(address: Address, lastTime: Long)
case class SynchronizeEvent(event: EventEnvelope)
case object SynchronizeEnd

class ClusterEventStoreActor(cluster: Cluster) extends Actor with Stash with ActorLogging {

  var sites: Set[Site] = Set.empty
  var isSelfUp: Boolean = false
  var isLeader: Boolean = false
  var leader: Option[Address] = None
  var isSynchronized: Boolean = false
  var events: List[EventEnvelope] = Nil

  def canAcceptMessage = isSelfUp && (isLeader || isSynchronized)

  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents, classOf[MemberEvent], classOf[LeaderChanged], classOf[UnreachableMember])
  }

  override def postStop(): Unit = cluster.unsubscribe(self)

  case class Add(event: EventEnvelope)
  case class SiteUp(site: Site)
  case class SiteRemove(site: Site)

  def mainReceive(): Receive = {

    implicit val ec = context.dispatcher

    {

      case SiteUp(site) =>
        log.info("Site is up")
        sites = sites + site
        if(site.member.address == cluster.selfAddress) {
          isSelfUp = true
        }

      case SiteRemove(site) =>
        log.info("Site is removed")
        sites = sites - site
        if(site.member.address == cluster.selfAddress) {
          isSelfUp = false
        }
      /**
        * MemberUp
        */
      case MemberUp(member) =>
        log.info("Member is Up: {}", member.address)
        ClusterUtils.searchActorInMember(self, member).foreach { memberRef =>
          self ! SiteUp(Site(member = member, ref = memberRef, isSelf = false, isLeader = false))
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
          self ! SiteRemove(site)
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
                log.info(s"Leader changed, so we need to synchronized")
                site.ref ! Synchronize
              }
            }
          case None =>
            sites = sites.map {_.copy(isLeader = false)}
        }
        isLeader = newLeader.contains(cluster.selfAddress)

      case Synchronize(address, lastTime) if isSelfUp && isLeader =>
        log.info(s"Synchronize request from: $address from: $lastTime")
        sites.find(_.member.address == address).foreach { site =>
          events.filter( _.time > lastTime).foreach { head =>
            site.ref ! SynchronizeEvent(head)
          }
          site.ref ! SynchronizeEnd
        }

      /**
        * Event if there is no selfMemberUp or leader
        */
      case event if !canAcceptMessage =>
        log.info(s"Stashing event: $event: $isSelfUp, $isLeader, $isSynchronized")
        stash()

      case memberEvent: ClusterDomainEvent =>
        println(memberEvent)

      case Add(event: EventEnvelope) =>
        log.info(s"Added: $event")
        events = event :: events

      case event if canAcceptMessage =>
        log.info(s"Received message: $event")
        val lastTime = events.headOption.map(_.time).getOrElse(0L)
        self ! Add(EventEnvelope(lastTime, event))


    }: Receive
  }

  override def receive: Receive = mainReceive()
}
