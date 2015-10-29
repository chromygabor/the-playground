package com.chromy.frpui.core

/**
 * Created by cry on 2015.10.29..
 */
trait BaseService extends BaseModel {
  type I

  def api(context: Context): I
}

trait Service[INTF, MODEL <: BaseModel] extends BaseService {
  type I = INTF
  type M = MODEL
}


trait ServiceBuilder[A <: BaseService] {
  def initialValue: A

  def key: String

  def clazz: Class[A#I]
}

object ServiceBuilder {

  case class ServiceAdded[B <: BaseService](serviceName: String, service: B) extends Action

  def singleton[A <: BaseService : Manifest](iInitialValue: A)(implicit ev: Manifest[A#I]): ServiceBuilder[A] = new ServiceBuilder[A] {
    val m = manifest[A#I]
    override lazy val initialValue: A = iInitialValue

    override lazy val key: String = {
      m.runtimeClass.getName
    }

    override val clazz = m.runtimeClass.asInstanceOf[Class[A#I]]
  }
}
