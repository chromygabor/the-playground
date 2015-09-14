package com.chromy.reactiveui.core

/**
 * Created by cry on 2015.09.13..
 */
case class Service[A <: BaseModel](store:  () => (Context[A], A)) {
}
