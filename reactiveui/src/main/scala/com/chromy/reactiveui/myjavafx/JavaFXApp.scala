package com.chromy.reactiveui.myjavafx

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui.Signal
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Scheduler => ScalaScheduler}
import rx.schedulers.Schedulers

/**
 * Created by chrogab on 2015.04.28..
 */

trait ModuleAction[M] {
  def step(model: M):  M
}

trait Module[M, C] {
  type Model = M
  type Controller = C

  class Actions(s: M => M) extends ModuleAction[M] {
    override def step(model: M): M = s(model)
  }

  def update(action: ModuleAction[M], model: M) : M = {
    action.step(model)
  }

  def apply(): (Parent, C) = {
    val clazzName = if(getClass.getSimpleName.endsWith("$")) getClass.getSimpleName.dropRight(1) else getClass.getSimpleName
    val loader = new FXMLLoader(getClass().getResource(s"$clazzName.fxml"))
    (loader.load(), loader.getController[C])
  }
}

object JavaFXScheduler {
  def apply() = {
    new ScalaScheduler {
      val asJavaScheduler = Schedulers.from(new Executor {
        override def execute(command: Runnable): Unit = Platform.runLater(command)
      })
    }
  }
}

object JavaFXApp extends App{
  val fxPanel = new JFXPanel()


  type MainModule = CounterPair
  type Model = CounterPair.Model
  val actions = Signal.mailbox[ModuleAction[Model]]

  val model = new Model

  Platform.runLater(new Runnable() {
    override def run(): Unit = {
      val (view, controller) = CounterPair()
      val stage = new Stage

      CounterPair.render(model, controller, actions.address)

      actions.signal.observeOn(ComputationScheduler()).scan(model) {(prevModel, action) =>
        val newModel = CounterPair.update(action, prevModel)
        println(s"model has changed: $newModel on: ${Thread.currentThread().getName}")
        newModel
      }.drop(1).observeOn(JavaFXScheduler()).subscribe(
      { model =>
        println(s"applying new model on: ${Thread.currentThread().getName}")
        CounterPair.render(model, controller, actions.address)
      }, { (error) => error.printStackTrace() }, { () => }
      )

      stage.setScene(new Scene(view))
      stage.setTitle("CounterPair App")
      stage.show()
    }
  })
}