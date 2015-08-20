package com.chromy.reactiveui

import com.chromy.reactiveui.core.{BaseModel, BaseComponent, Model}
import rx.lang.scala.schedulers.ImmediateScheduler

/**
 * Created by cry on 2015.07.12..
 */
trait BaseTest {

  implicit var list = List[Change[_]]()

  case class Change[M <: BaseModel](name: String, newModel: M)

  implicit class component2State[C <: BaseComponent{type ModelType <: BaseModel}](comp : C ) {
    def state: Option[C#ModelType] = {
      var state: C#ModelType = null.asInstanceOf[C#ModelType]
      val subscription = comp.router.changes.subscribeOn(ImmediateScheduler()).subscribe({lastState => state = lastState.asInstanceOf[C#ModelType]})
      subscription.unsubscribe()
      state match {
        case null => None
        case _ => Some(state)
      }
    }
    def prependToList(name: String): Unit = {
      comp.router.changes.subscribe({change =>
        list = Change(name, change) :: list
      })
    }
  }

}
