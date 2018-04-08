package com.chromy.microservice

import akka.http.scaladsl.model.HttpMethods

import scala.concurrent.Future

object TestService {
  implicit lazy val factory = Service[TestService]
}

class TestService extends Service {
  override protected val descriptor: ServiceDescriptor = ServiceDescriptor("TestService")
    .withRestCall(HttpMethods.GET, "/func0a/", func0A _)
    .withRestCall(HttpMethods.GET, "/func0b/", func0B _)
    .withRestCall(HttpMethods.GET, "/func2/:p1/:p2", func2 _)
    .withRestCall(HttpMethods.GET, "/func2/:p1/:p2/:p3", func3 _)


  def func0A: Call[NotUsed, String] = Call { _ =>
    Future.successful(s"Response from test: 0A")
  }
  def func0B(): Call[NotUsed, String] = Call { _ =>
    Future.successful(s"Response from test: 0B")
  }

  def func2(p1: Int, p2: String): Call[NotUsed, String] = Call { _ =>
    Future.successful(s"Response from test: $p1, $p2")
  }

  def func3(p1: Int, p2: String): Call[NotUsed, String] = Call { _ =>
    Future.successful(s"Response from test: $p1, $p2")
  }
}

