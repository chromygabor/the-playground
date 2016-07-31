package eventuate

import akka.actor.{ActorPath, ActorRef, ActorRefFactory, RootActorPath}
import akka.cluster.Member
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Created by Gábor on 2016.07.30..
  */
object ClusterUtils {

  /**
    * Return an actorRef for a member's same actor
    *
    * @param member
    * @return
    */
  def searchActorInMember(lookFor: ActorRef, member: Member)(implicit context: ActorRefFactory): Future[ActorRef] = {
    val pathWithoutAddress = lookFor.path.toStringWithoutAddress.split("/")
    val newActorPath = pathWithoutAddress.foldLeft(RootActorPath(member.address): ActorPath) { (path, part) =>
      path / part
    }
    implicit val resolveTimeout = Timeout(5.seconds)

    context.actorSelection(newActorPath).resolveOne()
  }
}
