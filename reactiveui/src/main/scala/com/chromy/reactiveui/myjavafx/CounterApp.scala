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

import scala.util.{Try, Failure, Success}


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

  def dispatch(component: Component): Component
}

object JavaFXModule {


  def componentType[A <: JavaFXModule : Manifest]: reflect.runtime.universe.Type = {
    import reflect.runtime.{currentMirror => mirror}
    import scala.reflect.runtime.universe._

    val m = typeOf[A].member("Component": TypeName)
    val tpe = typeOf[A]
    m.asType.toTypeIn(tpe)
  }

  def apply[M <: JavaFXModule : Manifest](component: M#Component): Try[(Parent, M, M#Component)] = {
    val ct = manifest[M]

    Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val node: Parent = loader.load()
      val controller = loader.getController[M]
      controller.dispatch(component.asInstanceOf[controller.Component])
      (node, controller, component)
    }
  }

  def apply[M <: JavaFXModule : Manifest](routerMapper: RouterMapper[M#Model], initialState: M#Model): Try[(Parent, M, M#Component)] = {
    import reflect.runtime.{currentMirror => mirror}
    import scala.reflect.runtime.universe._

    val ct = manifest[M]

    scala.util.Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val typeOfComponent = JavaFXModule.componentType[M]

      val ctor = typeOfComponent.typeSymbol.asClass.primaryConstructor

      val classMirror = mirror.reflectClass(typeOfComponent.typeSymbol.asClass)
      val component = classMirror.reflectConstructor(ctor.asMethod).apply(routerMapper, initialState).asInstanceOf[M#Component]

      val node: Parent = loader.load()
      val controller = loader.getController[M]
      controller.dispatch(component.asInstanceOf[controller.Component])
      (node, controller, component)
    }
  }

}

trait GenericJavaFXModule[A <: BaseComponent] extends JavaFXModule {
  type Controller = this.type
  type Model = A#ModelType
  type Component = A

}


object CounterApp extends App {

  import scala.util.{Try, Success, Failure}

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

      JavaFXModule[CountersController](router.mapper, initialState) match {
        case Success((parent, appController, appComponent)) => (parent, appController, appComponent)
          val stage = new Stage
          stage.setScene(new Scene(parent))
          stage.setTitle("CounterPair App")
          stage.show()
        case Failure(e) =>
          e.printStackTrace()
      }

    }


  })
}
