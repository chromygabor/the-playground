package com.chromy.microservice

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}

import scala.language.experimental.macros
import scala.language.implicitConversions

case object NotUsed {
  implicit def NotUsedToEntity: ToEntityMarshaller[NotUsed.type] = Marshaller.strict(_ => throw new IllegalAccessException("Invoker should not use NotUsed marshaller."))

  implicit def NotUserFromRequest: FromEntityUnmarshaller[NotUsed.type] = Unmarshaller.strict(_ => throw new IllegalAccessException("Invoker should not use NotUsed unmarshaller."))

}



