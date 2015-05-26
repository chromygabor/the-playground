package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.Atom
import com.chromy.reactiveui.Utils._

//package com.chromy.reactiveui.myjavafx
//
//import javafx.beans.property.StringProperty
//import javafx.fxml.FXML
//import javafx.scene.control.{Button, Label}
//import javafx.scene.layout.FlowPane
//
//import com.chromy.reactiveui.myjavafx.CounterPair._
//import com.chromy.reactiveui.{Signal}
//
///**
// * Created by chrogab on 2015.04.28..
// */
//
//case object Increment extends Action
//case object Decrement extends Action
//case object Clear extends Action
//
//case class CounterModel(counter: Int = 0)
//
//object Counter extends Module[CounterModel, Counter] {
//  override def update: (Action, CounterModel) => CounterModel = { (action, model) => action match {
//    case Increment => model.copy(counter =  model.counter + 1)
//    case Decrement => model.copy(counter =  model.counter - 1)
//    case Clear => model.copy(counter =  0)
//    case _ => model
//  }}
//
//  def render(model: Model, controller: Controller): Unit = {
//    controller.counterLabel.setText(model.counter.toString)
////    controller.incrementButton.setOnAction { () => model.address.onNext(Increment) }
////    controller.decrementButton.setOnAction { () => model.address.onNext(Decrement) }
//  }
//}
//

case class CounterModel(value: Int = 0)

object Counter {
  type Model = CounterModel
}

trait Change

case class GrandChildModel(value: Int = 0) extends Change

case class ChildModel(value: Int = 0, leftGrandChild: GrandChildModel = GrandChildModel(), rightGrandChild: GrandChildModel = GrandChildModel()) extends Change

case class ParentModel(child1: ChildModel = ChildModel(), child2: ChildModel = ChildModel()) extends Change

class Counter extends FlowPane {
  @FXML private var _counterLabel: Label = _
  @FXML private var _incrementButton: Button = _
  @FXML private var _decrementButton: Button = _

  def counterLabel = _counterLabel

  def incrementButton = _incrementButton

  def decrementButton = _decrementButton

  val model = ParentModel()

  val root = Atom(model) {
    case (oldModel, newModel) =>
      println(s"$oldModel => $newModel")
  }

  def createLotOfAtom(parent: Atom[ParentModel]): Unit = {
    for (i <- 0 to 1000) {
      val child1Atom = parent.map(_.child1)(newChild => _.copy(child1 = newChild))
      child1Atom.changes.subscribe({ model =>
        println(s"Child1 - $i change: $model")
      })
    }
  }


  def dispatch(parent: Atom[CounterModel]): Unit = {
    incrementButton.setOnAction { () =>
      createLotOfAtom(root)
    }
  }

  def render(model: CounterModel, input: Atom[CounterModel]) = {

  }
}