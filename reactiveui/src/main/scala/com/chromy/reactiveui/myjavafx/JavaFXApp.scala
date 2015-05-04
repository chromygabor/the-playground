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

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Promise, Future}
import scala.util.{Failure, Success, Try}

/**
 * Created by chrogab on 2015.04.28..
 */

trait ModuleAction[M] {
  def step(model: M):  Future[M]
}

trait Module[M, C] {
  type Model = M
  type Controller = C

  class Actions(s: M => Future[M]) extends ModuleAction[M] {
    override def step(model: M): Future[M] = s(model)
  }

  object Action {
    class Async(s: M => Future[M]) extends Actions( model => s(model))
    class Sync(s: M => M) extends Actions( {model =>
      val p = Promise[M]
      p.complete(Try{s(model)})
      p.future
    })
  }


  def update(action: ModuleAction[M], model: M) : Future[M] = {
    action.step(model)
  }

  def apply(): (Parent, C) = {
    val clazzName = if(getClass.getSimpleName.endsWith("$")) getClass.getSimpleName.dropRight(1) else getClass.getSimpleName
    val loader = new FXMLLoader(getClass().getResource(s"$clazzName.fxml"))
    (loader.load(), loader.getController[C])
  }
}

object JavaFXExecutionContext {
  val executionContext = ExecutionContext.fromExecutor(new Executor {
    override def execute(command: Runnable): Unit = Platform.runLater(command)
  })
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

      val f0 = model

      CounterPair.render(model, controller, actions.address)

      actions.signal.scan(f0) { (prevModel, action) =>
        val fn = Try{CounterPair.update(action, prevModel)}
        val r = fn.map {fn => Await.ready(fn, Duration.Inf)}

        r.flatMap {fut => fut.value.get.map {model => model}} match {
          case Success(model) => model
          case Failure(error) =>
            System.err.println("Exception should be catch in Update function!!!!")
            error.printStackTrace(System.err)
            prevModel
        }
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