package eventuate

import akka.actor._
import akka.cluster.ClusterEvent._
import akka.cluster.{Cluster, Member}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * Created by chrogab on 2016.07.14..
 */
object ClusterPlayground {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty)
      startup(Seq("2551", "2552", "0"))
    else
      startup(args)
  }

  def startup(ports: Seq[String]): Unit = {
    ports foreach { port =>
      // Override the configuration of the port
      val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
        withFallback(ConfigFactory.load())

      // Create an Akka system
      val system = ActorSystem("ClusterSystem", config)
      // Create an actor that handles cluster domain events
      system.actorOf(Props[SimpleClusterListener], name = "clusterListener")
    }
  }
}


case class Site(member: Member, ref: ActorRef)

case class Clock(counter: Long) {
  val currentTime = System.currentTimeMillis()

  def next: Clock = Clock(counter + (System.currentTimeMillis() - currentTime))
}

case object SiteTick

case class StampedEvent(timestamp: Long, event: Any)

/**
 * Actor
 */
class SimpleClusterListener extends Actor with ActorLogging {

  case object Tick

  private val cluster = Cluster(context.system)

  private var sites = Set.empty[Site]
  implicit private var clock: Clock = Clock(0)

  implicit val ec = context.dispatcher

  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents, classOf[MemberEvent], classOf[UnreachableMember])
  }

  override def postStop(): Unit = cluster.unsubscribe(self)

  /**
   * Return an actorRef for a member's same actor
   * @param member
   * @return
   */
  def actorRefForMember(member: Member): Future[ActorRef] = {
    val pathWithoutAddress = self.path.toStringWithoutAddress.split("/")
    val newActorPath = pathWithoutAddress.foldLeft(RootActorPath(member.address): ActorPath) { (path, part) =>
      path / part
    }
    implicit val resolveTimeout = Timeout(5.seconds)
    context.actorSelection(newActorPath).resolveOne()
  }


  def stamp(event: Any): StampedEvent = {
    clock = clock.next
    StampedEvent(clock.counter, event)
  }

  val tickTask = context.system.scheduler.schedule(5.seconds, 5.seconds, self, Tick)

  def receive = {
    // Sending out tick to sites
    case Tick =>
      sites.foreach { site =>
        site.ref ! stamp(SiteTick)
      }

    case SiteTick =>

    case StampedEvent(departedTime, event) =>
      //Received a tick from another site
      clock = clock.next
      if (departedTime > clock.counter) {
        clock = Clock(departedTime + 1)
        log.info(s"Modifying clock to $clock")
      } else {
        log.info(s"Clock is $clock")
      }
      self ! event

    case MemberUp(member) if member.address != cluster.selfAddress =>
      log.info("Member is Up: {}", member.address)
      actorRefForMember(member).foreach { memberRef =>
        sites = sites + Site(member, memberRef)
      }
    case UnreachableMember(member) if member.address != cluster.selfAddress =>
      log.info("Member detected as unreachable: {}", member)
      sites = sites.collect {
        case e@Site(m, _) if member != m => e
      }
    case MemberRemoved(member, previousStatus) if member.address != cluster.selfAddress =>
      log.info("Member is Removed: {} after {}", member.address, previousStatus)
      sites = sites.collect {
        case e@Site(m, _) if member != m => e
      }

    case _: MemberEvent => // ignore
  }
}
