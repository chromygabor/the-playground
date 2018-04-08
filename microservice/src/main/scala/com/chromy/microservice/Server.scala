package com.chromy.microservice

import java.util.regex.Pattern

import akka.http.scaladsl.model.{ContentTypes, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.{complete, extractRequestContext, onComplete, pathPrefix, provide, reject}
import akka.http.scaladsl.server._
import akka.http.scaladsl.unmarshalling.Unmarshaller

import scala.collection.immutable.Seq
import scala.reflect._
import scala.util.{Failure, Success}
import akka.http.scaladsl.server.Directives._


case class Server(private val services: Map[Class[_], ServiceServerDescriptor[_]] = Map.empty) {
  def bind[S <: Service : ClassTag](service: S): Server = {
    val sd = ServiceServerDescriptor[S](service)
    val ct = classTag[S].runtimeClass
    copy(services = services.updated(ct, sd))
  }

  def routeOf[S <: Service : ClassTag]: Route = {
    val ct = classTag[S].runtimeClass
    services.get(ct).map(_.route).getOrElse(throw new NoSuchElementException(s"Couldn't find descriptor for $ct"))
  }

  lazy val routes: Route = {
    services.map { case (_, sd) =>
      sd.route
    }.reduce( _ ~ _)
  }
}

case class ServiceServerDescriptor[S <: Service : ClassTag](service: S) {

  private val patterns = service.serviceDescriptor.calls.map { callInfo =>
    val paramTypes = callInfo.methodInfo.params.map { case ParamInfo(name, clzz, serializer) =>
      name -> s"(?<$name>\\\\w+)"
    }

    val pattern = paramTypes.foldLeft(s"${callInfo.path}") { case (accu, (name, r)) =>
      accu.replaceAll(s":$name", r)
    }.replaceAll("\\/", "\\\\/")

    callInfo -> Pattern.compile(pattern)
  }

  val requestToCall: Directive1[(CallInfo[_,_], Seq[(String, Any)], Any)] = {
    extractRequestContext.flatMap { request =>
      implicit val ec = request.executionContext
      implicit val mat = request.materializer

      val path = request.unmatchedPath.toString
      val method = request.request.method

      patterns.collect {
        case (callInfo, pattern) if callInfo.method == method => (callInfo.asInstanceOf[CallInfo[Any, Any]], pattern.matcher(path))
      }.collectFirst {
        case (callInfo, matcher) if matcher.find() => (callInfo, matcher)
      } match {
        case Some((callInfo, matches)) =>
          val paramMap = callInfo.methodInfo.params.map { case ParamInfo(name, clazz, serializer) =>
            name -> serializer.deserialize(matches.group(name))
          }
          if (paramMap.forall(_._2.isDefined)) {
            val r = paramMap.map { case (name, oValue) => name -> oValue.get }

            val um = callInfo.fromReq
            val res = um(request.request.entity)

            if (request.request.entity.getContentType() != ContentTypes.NoContentType) {
              onComplete(um(request.request.entity)) flatMap {
                case Success(value) => provide((callInfo, r,  value))
                case Failure(RejectionError(r)) ⇒ reject(r)
                case Failure(Unmarshaller.NoContentException) ⇒ reject(RequestEntityExpectedRejection)
                case Failure(Unmarshaller.UnsupportedContentTypeException(x)) ⇒ reject(UnsupportedRequestContentTypeRejection(x))
                case _ => reject
              }
            } else {
              provide((callInfo, r,  NotUsed))
            }
          }
          else {
            reject
          }
        case _ => reject
      }
    }
  }

  val serviceRoute = pathPrefix(s"${service.serviceDescriptor.name}") & requestToCall & extractExecutionContext

  def route: Route = serviceRoute  { case in@((callInfo, params, value), ec) =>
    onComplete(callInfo.invoke(service, value, params)(ec)) {
      case Success(me) => complete(HttpResponse(StatusCodes.OK, Nil, me))
      case Failure(error) => complete(HttpResponse(StatusCodes.BadRequest, Nil, error.toString))
    }
  }


}
