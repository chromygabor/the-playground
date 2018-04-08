package com.chromy.microservice

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.Future

class ServiceTest  extends WordSpec with Matchers with ScalatestRouteTest with ScalaFutures {

  def serviceOf[A <: Service : ServiceFactory](invoker: Invoker) = {
    val serviceFactory = implicitly[ServiceFactory[A]]
    serviceFactory.apply(invoker)
  }

  "TestService " should {
    "return a not handled response for invalid request" in {
      val invoker = new Invoker {
        override def invoke[Req, Res](callInfo: CallInfo[Req, Res], request: Req, args: Any*): Future[Res] = {
          callInfo.methodInfo.method.getName match {
            case "func2" =>
              Future.successful(s"$request, $args").asInstanceOf[Future[Res]]
            case e => Future.failed(new IllegalAccessException(s"This invoker doesn't support: $e"))
          }
        }
      }

      val testService = serviceOf[TestService](invoker)
      val f = testService.func2(10, "30").invoke(NotUsed)
      whenReady(f) { result =>
        result shouldBe "NotUsed, WrappedArray(10, 30)"
      }
    }
  }
}
