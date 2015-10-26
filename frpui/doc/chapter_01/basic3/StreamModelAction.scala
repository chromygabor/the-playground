package com.chromy.frpui.examples.basics.basic3

import rx.lang.scala.Subject

/**
 * Created by chrogab on 2015.10.22..
 */
trait Action
case class AddValueEmitted(value: Int) extends Action
case class SubValueEmitted(value: Int) extends Action

case class LeftModel(value: Int = 0) {
  def step(action: Action): LeftModel = action match {
    case AddValueEmitted(number) => LeftModel(value + number)
    case SubValueEmitted(number) => LeftModel(value + number)
  } 
}

case class RightModel(value: Int = 0) {
  def step(action: Action): RightModel = action match {
    case AddValueEmitted(number) => RightModel(value + number)
    case SubValueEmitted(number) => RightModel(value + number)
  }
}

case class MainModel(left: LeftModel = LeftModel(), right: RightModel = RightModel()) {
  def step(action: Action): MainModel = MainModel(left = left.step(action), right = right.step(action))
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

  app.s.onNext(AddValueEmitted(10))
  app.s.onNext(AddValueEmitted(20))
  app.s.onNext(AddValueEmitted(30))
  app.s.onNext(AddValueEmitted(40))
}
