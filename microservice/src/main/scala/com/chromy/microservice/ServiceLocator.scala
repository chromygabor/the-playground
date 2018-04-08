package com.chromy.microservice

import akka.http.scaladsl.model.Uri

import scala.concurrent.Future

trait ServiceLocator {
  def getServiceUri(serviceName: String, path: String): Future[Uri]
}
