package eventuate.eventstore

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.{StandardRoute, Route}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.{HttpResponse, StatusCode, StatusCodes}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.json.JsonFormat

import scala.io.StdIn

import scala.concurrent.Future
import scala.reflect.macros.whitebox
import scala.util.{Success, Failure}

/**
  * Created by chrogab on 2016.09.06..
  */

case class User(userId: Long, name: String)

trait RestServiceApp {
  def route: Route

  // needed to run the route
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // needed for the future map/flatmap in the end
  implicit val executionContext = system.dispatcher

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.terminate()) // and shutdown when done

}

object UserService extends RestServiceApp {

  sealed trait DomainResult[+T]

  trait FailureResultStatus extends DomainResult[Nothing] {
    def description: String
  }
  trait SuccessResultStatus[+T] extends DomainResult[T]

  case class NotFound[T](id: Long) extends FailureResultStatus {
    val description = s"Not found by id $id"
  }

  case object Done extends SuccessResultStatus[Nothing]
  case class Updated[T](value: T) extends SuccessResultStatus[T]
  case class Created[T](value: T) extends SuccessResultStatus[T]
  case class Found[T](value: T) extends SuccessResultStatus[T]

  implicit val userFormat = jsonFormat2(User)

  private[this] def fetchUser(id: Long): Future[DomainResult[User]] = Future {
    Found(User(1, ""))
  }

  private[this] def updateUser(user: User): Future[DomainResult[User]] = Future {
    Updated(user)
  }

  private[this] def createUser(user: User): Future[DomainResult[User]] = Future {
    Created(user)
  }

  def mapFailureToStatusCode(failure: FailureResultStatus): StatusCode = failure match {
    case NotFound(_) => StatusCodes.NotFound
    case _ => StatusCodes.InternalServerError
  }

  case class HttpErrorResult(failure: FailureResultStatus)

  def completeRequest[T](maybeItem: Future[DomainResult[T]])(implicit _marshaller: ToResponseMarshaller[T]) = {
    import spray.json._
    import DefaultJsonProtocol._

    implicit val userFormat = jsonFormat2(User)

    onComplete(maybeItem) {
      case Success(failure: FailureResultStatus) =>
        val s = failure.toJson

//        complete(HttpResponse(
//          status = mapFailureToStatusCode(failure),
//          entity = HttpErrorResult(failure)
//        ))
        ???
//      case Success(Right(user)) =>
//        complete(ToResponseMarshallable.apply(user))
//      case Failure(e) =>
//        complete(StatusCodes.InternalServerError)
    }

  }

  override def route: Route =
    get {
      pathPrefix("user" / LongNumber) { id =>
        // there might be no item for a given id
        completeRequest(fetchUser(id))
      }
    }

  //  ~ post {
  //    path("create-user") {
  //      entity(as[User]) { order =>
  //        val saved: Future[Done] = saveUser(order)
  //        onComplete(saved) { done =>
  //          complete("order created")
  //        }
  //      }
  //    }
  //  }

}

//class UserServiceActor extends PersistentActor with ActorLogging {
//  override def persistenceId: String = ???
//
//  override def receiveRecover: Receive = ???
//
//  override def receiveCommand: Receive = ???
//}
