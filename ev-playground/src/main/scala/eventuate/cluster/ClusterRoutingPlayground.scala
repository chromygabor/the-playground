package eventuate.cluster

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.RootActorPath
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.CurrentClusterState
import akka.cluster.ClusterEvent.MemberRemoved
import akka.cluster.ClusterEvent.MemberUp
import akka.cluster.Member
import akka.cluster.UniqueAddress
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory

import scala.util.Random

object ClusterRoutingPlayground  {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty)
      startup(Seq("2551", "2552", "2553"))
    else
      startup(args)
  }

  def startup(ports: Seq[String]): Unit = {
    val actors = ports foreach { port =>
      // Override the configuration of the port
      val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
        withFallback(ConfigFactory.load("cluster"))

      // Create an Akka system
      implicit val system = ActorSystem("ClusterSystem", config)
      implicit val materializer = ActorMaterializer()
      implicit val ec = system.dispatcher


      system.actorOf(Props[ClusterService], name = "consumer")
    }
  }

  case class Ready(address: UniqueAddress, clusterHash: Int)


  class ClusterService extends Actor {
    val cluster = Cluster(context.system)
    cluster.subscribe(self, classOf[MemberUp])


    val port = cluster.selfUniqueAddress.address.port.get

    var acked: Set[(Int, UniqueAddress)] = Set.empty
    var currentClusterHash = 0
    var expectedAcks: Set[(Int, UniqueAddress)] = Set.empty

    override def receive: Receive = {
      case Ready(member, clusterHash) =>
        acked = acked + (clusterHash -> member)


        println(s"[HL] [$port] received ready from: ${member.address.port.get} for $clusterHash\n\t[HL] acked: ${acked.map(_._2.address.port.get)}")

        if(expectedAcks == acked) {
          println(s"[HL] [$port] is in consesus for: $currentClusterHash")
        }

      case MemberUp(m) =>
        val clusterHash = cluster.state.members.hashCode()

        if(clusterHash != currentClusterHash) {
          changeHashing(cluster.state, clusterHash)
        }

        //if(m.hasRole())
      case MemberRemoved(m, _) =>
        val clusterHash = cluster.state.members.hashCode()

        if(clusterHash != currentClusterHash) {
          changeHashing(cluster.state, clusterHash)
        }
    }

    def changeHashing(clusterState: CurrentClusterState, clusterHash: Int): Unit = {
      //Do something
      val oldClusterHash = currentClusterHash
      currentClusterHash = clusterHash
      Thread.sleep(Random.nextInt(1000))

      expectedAcks = clusterState.members.map { member =>
        currentClusterHash -> member.uniqueAddress
      }

      println(s"[HL] [$port] changed to $currentClusterHash: ${acked.map(_._2.address.port)}")

      acked = acked.filter { case (hash, _) => hash == currentClusterHash}

      clusterState.members.foreach { member =>
        context.actorSelection(RootActorPath(member.address) / "user" / "consumer") ! Ready(cluster.selfUniqueAddress, currentClusterHash)
      }
    }
  }


}
