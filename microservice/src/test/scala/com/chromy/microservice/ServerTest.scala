package com.chromy.microservice

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}

class ServerTest extends WordSpec with Matchers with ScalatestRouteTest {

  val server = Server()
    .bind[TestService](new TestService)

  "TestService " should {
    "return a not handled response for invalid request" in {
      Get("/TestService/foo/bar") ~> server.routes ~> check {
        handled shouldBe false
      }
    }

    "return a valid response for func0a" in {
      Get("/TestService/func0a") ~> server.routes ~> check {
        responseAs[String] shouldEqual "Response from test: 0A"
      }
    }
    "return a valid response for func0a with a trailing slash" in {
      Get("/TestService/func0a/") ~> server.routes ~> check {
        responseAs[String] shouldEqual "Response from test: 0A"
      }
    }

    "return a valid response for func0B()" in {
      Get("/TestService/func0b/") ~> server.routes ~> check {
        responseAs[String] shouldEqual "Response from test: 0B"
      }
    }
    "return a valid response for Func2" in {
      Get("/TestService/func2/10/20") ~> server.routes ~> check {
        responseAs[String] shouldEqual "Response from test: 10, 20"
      }
    }
    "return a invalid response for Func3" in {
      Get("/TestService/func3/10/20") ~> server.routes ~> check {
        handled shouldBe false
      }
    }
  }

}
