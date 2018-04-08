//package com.chromy.microservice.persitence
//
//import akka.actor.{ActorContext, ActorRef, ActorSystem, Props}
//import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
//import akka.util.Timeout
//import com.chromy.concept.ShardingEntity
//import com.chromy.persistentservice.{PersistentServiceActor, PersistentServiceBase}
//import com.typesafe.config.Config
//
//import scala.concurrent.Future
//import scala.reflect.{ClassTag, classTag}
//
//trait ShardingEntity {
//
//}
//
//
//case class ShardingProxy(entityId: String, shardingActor: ActorRef) {
//  import akka.pattern._
//  import scala.concurrent.duration._
//
//  implicit val timeout = Timeout(10.seconds)
//  implicit val ec = scala.concurrent.ExecutionContext.global
//  def send(msg: Any): Unit = ??? //shardingActor ! ShardingActor.EntityEnvelope(entityId, msg)
//  def ask(msg: Any): Future[Any] = ??? //shardingActor.ask(ShardingActor.EntityEnvelope(entityId, msg))
//}
//
//object ShardingActor {
//  def props(typeName: String, serviceFactory: PersistentServiceFactory, config: Config) = {
//    def creator = serviceFactory.create[PersistentServiceBase](_, _)
//    Props(new PersistentServiceActor(creator, config))
//  }
//
//  val idExtractor: ShardRegion.ExtractEntityId = {
//    case e@ShardingActor.Init(persistenceId) =>
//      (persistenceId, e)
//    case ShardingActor.EntityEnvelope(persistenceId, payload) =>
//      (persistenceId, payload)
//  }
//
//  val shardResolver: ShardRegion.ExtractShardId = {
//    case e@ShardingActor.Init(persistenceId) =>
//      val id = (math.abs(persistenceId.hashCode) % 100).toString
//      id
//
//    case ShardingActor.EntityEnvelope(persistenceId, payload) =>
//      val id = (math.abs(persistenceId.hashCode) % 100).toString
//      id
//
//  }
//
//  case class Init(entityId: String)
//  case class EntityEnvelope(persistenceId: String, payload: Any)
//}
//
//
////class ShardingActor(typeName: String) extends PersistentActor {
////  override def receiveRecover: Receive = ???
////
////  override def receiveCommand: Receive = ???
////
////  override def persistenceId: String = typeName + "-" + self.path.name
////}
//trait PersistentServiceFactory {
//  def create[T <: PersistentServiceBase : ClassTag](actorContext: ActorContext, actorRef: ActorRef): T
//}
//
//class Sharding(system: ActorSystem, serviceFactory: PersistentServiceFactory, config: Config) {
//  val clusterSharding = ClusterSharding(system)
//
//  var startedShards = Set.empty[String]
//
//  def refFor[A <: ShardingEntity : ClassTag](entityId: String): ShardingProxy = {
//    val ct = classTag[A]
//    val typeName = ct.runtimeClass.getCanonicalName
//    val actorRef = if(!startedShards(typeName)) {
//      clusterSharding.start(
//        typeName = typeName,
//        entityProps = ShardingActor.props(typeName, serviceFactory, config),
//        settings = ClusterShardingSettings(system),
//        extractEntityId = ShardingActor.idExtractor,
//        extractShardId = ShardingActor.shardResolver)
//    } else {
//      clusterSharding.shardRegion(typeName)
//    }
//    actorRef ! ShardingActor.Init(entityId)
//    ShardingProxy(entityId, actorRef)
//  }
//}