package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.10.29..
 */
trait BaseService extends BaseModel {
  type I

  def api(context: UpdateContext): I
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

  def apply[A <: BaseService : Manifest](iInitialValue: (String) => A)(implicit ev: Manifest[A#I]): ServiceBuilder[A] = new ServiceBuilder[A] {
    val m = manifest[A#I]

    override lazy val key: String = {
      m.runtimeClass.getName
    }

    override lazy val initialValue: A = iInitialValue(key)

    override val clazz = m.runtimeClass.asInstanceOf[Class[A#I]]
  }
}
