package eventuate

import java.util.UUID

import akka.actor.Actor.Receive
import akka.actor.{ActorSystem, _}
import akka.cluster.ClusterEvent._
import akka.cluster.{Cluster, Member}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern._
import akka.persistence.PersistentActor
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by chrogab on 2016.07.14..
 */

case class TestMessage(e: String)

object ClusterPlayground {
  def route(clusterBroadcast: ActorRef)(implicit ec: ExecutionContext): Route = {
    implicit val timeout = Timeout(5.seconds)
    get {
      pathPrefix("broadcast") {
        implicit val timeout = Timeout(15.seconds)
        val r = clusterBroadcast ? TestMessage
        onSuccess(r) { in =>
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"<h1>Broadcast result is $in</h1>"))
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty)
      startup(Seq("2551", "2552", "0"))
    else
      startup(args)
  }

  def startup(ports: Seq[String]): Unit = {
    val actors = ports foreach { port =>
      // Override the configuration of the port
      val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
        withFallback(ConfigFactory.load())

      // Create an Akka system
      implicit val system = ActorSystem("ClusterSystem", config)
      implicit val materializer = ActorMaterializer()
      implicit val ec = system.dispatcher

      // Create an actor that handles cluster domain events
      val clusterBroadcast = system.actorOf(Props[ClusterBroadcast], name = "clusterListener")

      Http().bindAndHandle(route(clusterBroadcast), "localhost", port.toInt + 5530)
        .foreach { http =>
          println(s"Server is up on: ${http.localAddress}")
        }
    }
  }
}

case class StampedEvent(timestamp: Long, event: Any, uid: String, siteUid: String)

case class ConfirmStampedEvent(uid: String, siteUid: String, receiveTimestamp: Long)

case object Sent

case object Tick

object ClusterBroadcast {
  case class Site(member: Member, ref: ActorRef, uid: String = UUID.randomUUID().toString)

  case class Clock(counter: Long) {
    val currentTime = System.currentTimeMillis()

    def next: Clock = Clock(counter + (System.currentTimeMillis() - currentTime))
  }

  case class EventConfirmations(confirmedSites: Map[Site, Boolean], sender: ActorRef) {
    lazy val sites = confirmedSites.keySet

    def confirm(site: Site): Either[EventConfirmations, ActorRef] = {
      val newConfirmedSites = confirmedSites.updated(site, true)

      if(newConfirmedSites.exists(!_._2)) {
        Left(copy(confirmedSites = newConfirmedSites))
      } else {
        Right(sender)
      }
    }
  }

  object EventConfirmations {
    def apply(sites: Set[Site], sender: ActorRef): EventConfirmations = {
      new EventConfirmations(confirmedSites = sites.map(_ -> false).toMap, sender = sender)
    }
  }
}

/**
 * Actor
 */
class ClusterBroadcast extends PersistentActor with ActorLogging {
  import ClusterBroadcast._
  private[this] val cluster = Cluster(context.system)
  private[this] var sites = Set.empty[Site]
  private[this] implicit var clock: Clock = Clock(0)
  private[this] implicit val ec = context.dispatcher
  private[this] var tickTask: Cancellable = _
  private[this] var confirmations: Map[String, EventConfirmations] = Map.empty

  case object Init
  
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
        EventConfirmations(sites, context.actorOf(Props(new Actor {
          override def receive: Receive = {
            case Sent =>
          }
        })))
      case _ => 
        EventConfirmations(sites, eventSender)
    }
    confirmations = confirmations.updated(uid, eventConfirmations)

    log.debug(s"Sending: $event to ${eventConfirmations.sites}")
    eventConfirmations.sites.foreach { site =>
      val stampedEvent = StampedEvent(clock.counter, event, uid, site.uid)
      site.ref ! stampedEvent
    }
  }

  private def sendToSubscribers(event: Any): Unit = {

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

  override def receive = {
    // Got a message from other node's ClusterBroadcast actor 
    case StampedEvent(departedTime, event, uid, site) =>
      synchronizeClock(departedTime)
      sender() ! ConfirmStampedEvent(uid, site, clock.counter)
      event match {
        case Tick =>
          //Tick is only for synchronization
        case event => 
          sendToSubscribers(event)
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

    case event => sendEventToSites(event, sender())

  }

  override def receiveRecover: Receive = {
    case e => println(s"receiveRecover: $e")
  }

  override def receiveCommand: Receive = {
    case e => 
      println(s"receiveCommand: $e")
  }

  override def persistenceId: String = "clusterBroadcast"
  
}
