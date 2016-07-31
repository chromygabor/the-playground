package eventuate

import java.util.UUID

import akka.actor.ActorRef
import akka.cluster.Member

/**
  * Created by Gábor on 2016.07.30..
  */
case class Site(member: Member, ref: ActorRef, isSelf: Boolean = false, isLeader: Boolean = false, uid: String = UUID.randomUUID().toString)
