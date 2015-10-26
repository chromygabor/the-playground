package com.chromy.frpui.examples.basics.basic4

import rx.lang.scala.Subject

/**
 * Created by chrogab on 2015.10.22..
 */
trait Action
case class AddValueEmitted(id: Int, value: Int) extends Action
case class SubValueEmitted(id: Int, value: Int) extends Action

case class SubModel(id: Int, value: Int = 0) {
  def step(action: Action): SubModel = action match {
    case AddValueEmitted(id, number) => SubModel(value + number)
    case SubValueEmitted(id, number) => SubModel(value + number)
  } 
}

object MainModel {
  val LeftID = 0
  val RightID = 1
}

case class MainModel(left: SubModel = SubModel(MainModel.LeftID), right: SubModel = SubModel(MainModel.RightID)) {
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


object StreamModelActionWithUid extends App {
  val app = new AppStream(MainModel())

  app.s.onNext(AddValueEmitted(MainModel.LeftID, 50))
  app.s.onNext(AddValueEmitted(MainModel.RightID, 40))
  app.s.onNext(SubValueEmitted(MainModel.LeftID, 10))
  app.s.onNext(SubValueEmitted(MainModel.RightID, 15))
}
