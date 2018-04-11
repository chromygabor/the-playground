package com.chromy.soundcloud
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.pathPrefix
import akka.stream.ActorMaterializer
import com.chromy.microservice.CallInfo
import com.chromy.microservice.Invoker
import com.chromy.microservice.Service
import com.chromy.microservice.guice.ServiceRoutes
import com.google.inject.Guice
import com.google.inject.name.Names
import com.typesafe.scalalogging.LazyLogging
import net.codingwell.scalaguice.ScalaModule

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.io.StdIn

trait ActorSystemSupport extends MyServiceModule with LazyLogging {

  abstract override def configure(): Unit = {
    println("Binding ActorSystem")

    bind[ActorSystem].
      toInstance(actorSystem)

    super.configure()
  }
}

trait MyServiceModule extends ScalaModule with LazyLogging  {
  protected implicit val actorSystem: ActorSystem = ActorSystem(name = "my-system")
  protected implicit val materializer: ActorMaterializer = ActorMaterializer()
  protected implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  private var services: Set[Manifest[_ <: Service]] = Set.empty

  private var alreadyBound = false

  protected def apiPrefix = "api"

  protected def start[T <: Service: Manifest] = {
    if(!alreadyBound) {
      bind[ServiceRoutes].toInstance(new ServiceRoutes(services))
      alreadyBound = true
    }
    services = services + manifest[T]
    bind[T]
  }

  def configure(): Unit = {
    println("Configure")
    initialize()
  }

  def main(args: Array[String]): Unit = {
    val inj = Guice.createInjector(this)
    if(alreadyBound) {
      println("Starting services")
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
  }

  def initialize(): Unit
}

object MyModule extends MyServiceModule with ActorSystemSupport {
  implicit val invoker2 = new Invoker {
    override def invoke[Req, Res](callInfo: CallInfo[Req, Res], request: Req, args: Any*): Future[Res] = {
      //      implicit val clazzTag: ClassTag[Res] = callInfo.clazzTag
      //      implicit val marshaller: Marshaller[Req, MessageEntity] = callInfo.marshaller
      println(s"Invoker has been called with: $callInfo, $request, $args")
      ???
    }
  }

  override def initialize(): Unit = {
    println("MyModule configure")

    bind[SoundCloudService]
      .annotatedWith(Names.named("soundcloudService"))
      .toInstance(Service.facade[SoundCloudService])

    start[SoundCloudServiceImpl]

  }
}

