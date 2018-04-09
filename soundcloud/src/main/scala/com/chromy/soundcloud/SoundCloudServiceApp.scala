package com.chromy.soundcloud

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives.pathPrefix
import akka.stream.ActorMaterializer
import com.chromy.microservice.guice.{ServiceModule, ServiceRoutes}
import com.chromy.microservice._
import com.google.inject.Guice
import com.google.inject.name.Names
import com.typesafe.config.ConfigFactory
import net.codingwell.scalaguice.ScalaModule

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

trait SoundCloudService extends Service {
  val descriptor = ServiceDescriptor("SoundCloud")
    .withRestCall(HttpMethods.GET, s"/foo", foo _)

  protected def foo(param1: Long, param2: String): Call[NotUsed, String]

}

class SoundCloudServiceImpl extends SoundCloudService {
  override protected def foo(param1: Long, param2: String): Call[NotUsed, String] = ???
}

object ServiceAppModule extends ScalaModule with ServiceModule with ServiceModuleApp {
  override def configure(): Unit = {

    implicit val invoker2 = new Invoker {
      override def invoke[Req, Res](callInfo: CallInfo[Req, Res], request: Req, args: Any*): Future[Res] = {
        //      implicit val clazzTag: ClassTag[Res] = callInfo.clazzTag
        //      implicit val marshaller: Marshaller[Req, MessageEntity] = callInfo.marshaller
        println(s"Invoker has been called with: $callInfo, $request, $args")
        ???
      }
    }

    bind[ActorSystem].
      toInstance(actorSystem)

    bind[SoundCloudService]
      .annotatedWith(Names.named("soundcloudService"))
      .toInstance(Service.facade[SoundCloudService])

    start[SoundCloudServiceImpl]
  }
}

trait ServiceModuleApp extends App {
  this: ScalaModule with ServiceModule =>
  protected val config = ConfigFactory.load()
  protected val apiPrefix = "api"


  protected implicit val actorSystem: ActorSystem = ActorSystem(name = "my-system", config = config)
  protected implicit val materializer: ActorMaterializer = ActorMaterializer()
  protected implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  val inj = Guice.createInjector(this)
  val myServer = inj.getInstance(classOf[ServiceRoutes])

  def route =
    pathPrefix(apiPrefix) {
      myServer.routes(inj)
    }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 9000)

  println(s"Server online at http://localhost:9000/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => actorSystem.terminate()) // and shutdown when done

}