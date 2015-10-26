package com.chromy.frpui.examples.basics.basic1

import rx.lang.scala.Subject

/**
 * Created by chrogab on 2015.10.22..
 */

case class LeftModel(value: Int = 0) {
  def copy(newValue: Int): LeftModel = LeftModel(value + newValue)
}

case class RightModel(value: Int = 0) {
  def copy(newValue: Int): RightModel = RightModel(value + newValue)
}

case class MainModel(left: LeftModel = LeftModel(), right: RightModel = RightModel()) {
  def copy(newValue: Int): MainModel = MainModel(left = left.copy(newValue), right = right.copy(newValue))
}

class AppStream[T <: {def copy(newValue : Int) : T}](initialValue: T) {
  val s = Subject[Int]()

  s.scan(initialValue) { case (model, item) =>
    model.copy(item)
  }.foreach { model =>
    println(model)
  }

}

object StreamModel extends App {
  val app = new AppStream(MainModel())

  app.s.onNext(10)
  app.s.onNext(20)
  app.s.onNext(30)
  app.s.onNext(40)
}
