package com.chromy.frpui.examples.basics

import rx.lang.scala.Subject

/**
 * Created by chrogab on 2015.10.22..
 */
trait Action
case class AddNumber(value: Int) extends Action
case class SubNumber(value: Int) extends Action

case class LeftModel(value: Int = 0) {
//  def copy(action: Action): LeftModel = action match {
//    LeftModel(value + newValue)
//    
//  } 
    
}

case class RightModel(value: Int = 0) {
  def copy(newValue: Int): RightModel = RightModel(value + newValue)
}

case class MainModel(left: LeftModel = LeftModel(), right: RightModel = RightModel()) {
  def copy(newValue: Int): MainModel = MainModel(left = left.copy(newValue), right = right.copy(newValue))
}

class AppStream[T <: {def step(action : Action) : T}](initialValue: T) {
  val s = Subject[Action]()

  s.scan(initialValue) { case (model, item) =>
    model.step(item)
  }.foreach { model =>
    println(model)
  }
}


object StreamModelAction extends App {
  val app = new AppStream(MainModel())

  app.s.onNext(10)
  app.s.onNext(20)
  app.s.onNext(30)
  app.s.onNext(40)
}
