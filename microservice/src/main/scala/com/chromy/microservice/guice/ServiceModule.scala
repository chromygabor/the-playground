package com.chromy.microservice.guice

import akka.http.scaladsl.server.Directives._
import com.chromy.microservice.{Service, ServiceServerDescriptor}
import com.google.inject.Injector
import net.codingwell.scalaguice.ScalaModule


class ServiceRoutes(services: => Set[Manifest[_ <: Service]] = Set.empty) {
  def routes(injector: Injector) = {
    services.map { service =>
      val serviceInst = injector.getInstance(service.runtimeClass).asInstanceOf[Service]
      ServiceServerDescriptor(serviceInst).route
    }.reduce ( _ ~ _)
  }
}

trait ServiceModule  {
  this: ScalaModule =>


  protected var services: Set[Manifest[_ <: Service]] = Set.empty

  private var alreadyBound = false

  protected def start[T <: Service: Manifest] = {
    if(!alreadyBound) {
      bind[ServiceRoutes].toInstance(new ServiceRoutes(services))
      alreadyBound = true
    }
    services = services + manifest[T]
    bind[T]
  }
}
