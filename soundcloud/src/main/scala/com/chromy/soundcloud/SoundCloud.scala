package com.chromy.soundcloud

import akka.actor.ActorSystem

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object SoundCloud extends App {

  val CLIENT_ID = "ZJqyxkmTPZPDW1ytOfwCvqzGRZEFqUf2"
  val APP_VERSION = "1522759048"
  val APP_LOCALE = "EN"
  val SIGNIN_CLIENT_ID = "qPtWURX3JrkpXGy7vWetJDsiZVcOdpXy"

  val IDENTIFIER = "chromygabor"
  val PASSWORD = "C4h2r2o6m7y!"

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val uri = Uri(path = Path("/sign-in/password"), queryString = Some(s"client_id=$CLIENT_ID&app_version=$APP_VERSION&app_locale=$APP_LOCALE"))

  val payload = s"""{"client_id":"$SIGNIN_CLIENT_ID","scope":"fast-connect non-expiring purchase signup upload","recaptcha_pubkey":"6LeAxT8UAAAAAOLTfaWhndPCjGOnB54U1GEACb7N","recaptcha_response":"","credentials":{"identifier":"$IDENTIFIER","password":"$PASSWORD"},"signature":"7:33-1-49856-372-4953600-1054:5be2fa","device_id":"949514-845429-250228-390538","user_agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36"}"""

  val req = HttpRequest(uri = uri,
    method = HttpMethods.POST,
    entity = HttpEntity(contentType = ContentTypes.`application/json`, string = payload)
  ).withHeaders(
    RawHeader("client_id", CLIENT_ID),
    RawHeader("app_version", APP_VERSION),
    RawHeader("app_locale", APP_LOCALE)
  )

  println(req.toString())

  val connectionFlow: Flow[HttpRequest, HttpResponse, Future[Http.OutgoingConnection]] =
    Http().outgoingConnectionHttps("api-v2.soundcloud.com")
  val responseFuture: Future[HttpResponse] =
    Source.single(req)
      .via(connectionFlow)
      .runWith(Sink.head)

  responseFuture.andThen {
    case Success(r) => println(s"request succeded: $r")
    case Failure(_) => println("request failed")
  }.andThen {
    case _ => system.terminate()
  }

}

trait Requester {
  def doRequest(request: HttpRequest): Future[HttpResponse]
}
case class AuthRequest() extends Requester {
  override def doRequest(request: HttpRequest): Future[HttpResponse] = ???
}