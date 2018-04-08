package com.chromy.microservice

import scala.util.Try

object ParamSerializer {
  def apply[T](_serialize: T => String, _deserialize: String => T): ParamSerializer[T] = new ParamSerializer[T] {
    override def serialize(in: T): String = _serialize(in)

    override def deserialize(in: String): Option[T] = Try(_deserialize(in)).toOption
  }

  implicit val longSerializer: ParamSerializer[Long] = ParamSerializer(_.toString,_.toLong)
  implicit val intSerializer: ParamSerializer[Int] = ParamSerializer(_.toString,_.toInt)
  implicit val booleanSerializer: ParamSerializer[Boolean] = ParamSerializer(_.toString,_.toBoolean)
  implicit val stringSerializer: ParamSerializer[String] = ParamSerializer(identity, identity)
}

trait ParamSerializer[T] {
  def serialize(in: T): String

  def deserialize(in: String): Option[T]
}