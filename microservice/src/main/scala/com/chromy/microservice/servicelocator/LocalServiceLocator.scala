package com.chromy.microservice.servicelocator

import akka.http.scaladsl.model.Uri
import com.chromy.microservice.ServiceLocator
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future

class LocalServiceLocator() extends ServiceLocator{
  override def getServiceUri(serviceName: String, path: String): Future[Uri] =
    Future.successful(s"http://localhost:8080/$serviceName/$path")
}
