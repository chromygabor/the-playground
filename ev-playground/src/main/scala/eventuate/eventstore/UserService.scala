package eventuate.eventstore

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.{HttpResponse, StatusCode, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directives, Route}
import akka.stream.{ActorMaterializer, Materializer}
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.DefaultFormats
import org.json4s.{DefaultFormats, jackson}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.Success

/**
  * Created by chrogab on 2016.09.06..
  */

case class User(userId: Long, name: String)

trait RestServiceApp {
  def route(implicit ec: ExecutionContext, mat: Materializer): Route

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

  implicit val serialization = jackson.Serialization // or native.Serialization
  implicit val formats = DefaultFormats

  def route(implicit ec: ExecutionContext, mat: Materializer) = {
    import Directives._
    import Json4sSupport._

    implicit val serialization = jackson.Serialization // or native.Serialization
    implicit val formats = DefaultFormats

    pathSingleSlash {
      get {
        pathPrefix("user" / LongNumber) { id =>
          // there might be no item for a given id
          completeRequest(fetchUser(id))
        }
      } ~
      post {
        entity(as[User]) { foo =>
          complete {
            ???
          }
        }
      }
    }
  }

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
    onComplete(maybeItem) {
      case Success(failure: FailureResultStatus) =>

        ???
//      case Success(Right(user)) =>
//        complete(ToResponseMarshallable.apply(user))
//      case Failure(e) =>
//        complete(StatusCodes.InternalServerError)
    }

  }

//  override def route: Route =
//    get {
//      pathPrefix("user" / LongNumber) { id =>
//        // there might be no item for a given id
//        completeRequest(fetchUser(id))
//      }
//    }
//
//  //  ~ post {
//  //    path("create-user") {
//  //      entity(as[User]) { order =>
//  //        val saved: Future[Done] = saveUser(order)
//  //        onComplete(saved) { done =>
//  //          complete("order created")
//  //        }
//  //      }
//  //    }
//  //  }
//
}

//class UserServiceActor extends PersistentActor with ActorLogging {
//  override def persistenceId: String = ???
//
//  override def receiveRecover: Receive = ???
//
//  override def receiveCommand: Receive = ???
//}
