package com.chromy.reactiveui.myjavafx

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Scene, Parent}
import javafx.stage.Stage

import com.chromy.reactiveui.Dispatcher
import monocle.macros.GenLens
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Subject, Observable, Observer}
import rx.lang.scala.{Scheduler => ScalaScheduler}
import rx.schedulers.Schedulers

import scala.util.{Failure, Success, Try}

/**
 * Created by chrogab on 2015.06.04..
 */
trait Action
trait LocalAction extends Action

trait Module[M, C] {
  type Model = M
  type Controller = C

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

object CounterApp extends App {
  val fxPanel = new JFXPanel()

  case class AppModel(model: CountersModel = CountersModel())
  case object Nop extends Action

  Platform.runLater(new Runnable() {
    override def run(): Unit = {
      val (appComponent, appController) = Counters()

      val root = Dispatcher[CountersModel, Action]

      val actions = Subject[Action]
      val changes = Subject[CountersModel]

      val initModel = CountersModel()
      val stream = actions.observeOn(ComputationScheduler()).scan(initModel) { (oldState, action) =>
        Try{
          val newState = root.update(action).run(oldState)._1
          changes.onNext(newState)
          newState
        } match {
          case Success(newState) => newState
          case Failure(error) =>
            error.printStackTrace()
            oldState
        }
      }

      actions.subscribe({ in => println(s"action: $in")})
      changes.subscribe({in => println(s"changes: $in")})
      stream.subscribe({ in => println(s"stream: $in")})

      appController.dispatch(root.factory, actions, changes.observeOn(JavaFXScheduler()).distinctUntilChanged)

      actions.onNext(Nop)
      val stage = new Stage
      stage.setScene(new Scene(appComponent))
      stage.setTitle("CounterPair App")
      stage.show()
      println("after show")
    }
  })

}
