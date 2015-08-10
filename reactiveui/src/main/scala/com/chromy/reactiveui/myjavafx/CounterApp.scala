package com.chromy.reactiveui.myjavafx

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui.core._
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Scheduler => ScalaScheduler, Subject}
import rx.schedulers.Schedulers

import scala.util.{Failure, Success, Try}

/**
 * Created by chrogab on 2015.06.04..
 */

object JavaFXScheduler {
  def apply() = {
    new ScalaScheduler {
      val asJavaScheduler = Schedulers.from(new Executor {
        override def execute(command: Runnable): Unit = Platform.runLater(command)
      })
    }
  }
}


sealed trait JavaFXModule {
  type Controller
  type Model
  type Component

  def dispatch(routerMapper: RouterMapper[Model], initialState: Model): Component
}

trait GenericJavaFXModule[A <: Component] extends JavaFXModule {
  type Controller = this.type
  type Model = A#ModelType
  type Component = A

}


object JavaFXFactory {
  import scala.reflect.runtime.universe._
  import reflect.runtime.{currentMirror=>mirror}

  def apply[M <: Component : Manifest](component: M): Unit = ???

  def apply[M <: JavaFXModule : Manifest](routerMapper: RouterMapper[M#Model], initialState: M#Model): (Parent, M, M#Component) = {
    val ct = manifest[M]

    val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
    val loader = new FXMLLoader(getClass().getResource(s"$clazzName.fxml"))

    val node: Parent = loader.load()
    val controller = loader.getController[M]
    val component: M#Component = controller.dispatch(routerMapper.asInstanceOf[RouterMapper[controller.Model]], initialState.asInstanceOf[controller.Model])
    (node, controller, component)
  }

  trait ComponentFactory {
    def apply(param: Int): Component
  }

}


object CounterApp extends App {

  type C = Counters
  type M = CountersModel

  val initialState = CountersModel()

  val fxPanel = new JFXPanel()


  Platform.runLater(new Runnable() {
    override def run(): Unit = {

        lazy val name = s"DSP-MainComponent"
        println(s"[$name] created ")

        val router = new Router[M] {
          override val changes = BehaviorSubject[M]
          override val chain: UpdateChain[M] = UpdateChain()
          override val channel = Subject[Action]

          val stream = channel.scan(initialState) { (oldState, action) =>
            Try {
              println(s"=================== [$name] action received  $action =================")
              val newState = chain.update(action, oldState)
              println(s"[$name] - An action received in the main loop: $action -- $oldState => $newState")
              newState
            } match {
              case Success(newState) => newState
              case Failure(error) =>
                error.printStackTrace()
                oldState
            }
          }

          stream.drop(1) subscribe ({ newState =>
            println(s"[$name] - A publishing a change: $newState")
            changes.onNext(newState)
          })
        }

        val (parent, appController, appComponent) = JavaFXFactory[CountersController](router.mapper, initialState)

        val stage = new Stage
        stage.setScene(new Scene(parent))
        stage.setTitle("CounterPair App")
        stage.show()
    }


  })
}
