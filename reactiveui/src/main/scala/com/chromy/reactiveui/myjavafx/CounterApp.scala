package com.chromy.reactiveui.myjavafx

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui.core._
import monocle.macros.GenLens
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Scheduler => ScalaScheduler, Observer, Subject}
import rx.schedulers.Schedulers

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Failure, Success}


/**
 * Created by chrogab on 2015.06.04..
 */

object JavaFXScheduler {

  lazy val executor = new Executor {
    override def execute(command: Runnable): Unit = Platform.runLater(command)
  }

  def apply() = {
    new ScalaScheduler {
      val asJavaScheduler = Schedulers.from(executor)
    }
  }
}


sealed trait JavaFXModule {
  type Controller
  type Model
  type Component

  def dispatch(component: Component): Component
}

case class JavaFxModuleWrapper[A <: JavaFXModule](parent: Parent, controller: A, component: A#Component)

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

  def apply[M <: JavaFXModule : Manifest](contextMapper: ContextMapper[M#Model], initialState: M#Model): Try[(Parent, M, M#Component)] = {
    import reflect.runtime.{currentMirror => mirror}
    import scala.reflect.runtime.universe._

    val ct = manifest[M]

    scala.util.Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val typeOfComponent = JavaFXModule.componentType[M]

      val ctor = typeOfComponent.typeSymbol.asClass.primaryConstructor

      val classMirror = mirror.reflectClass(typeOfComponent.typeSymbol.asClass)
      val component = classMirror.reflectConstructor(ctor.asMethod).apply(contextMapper, initialState).asInstanceOf[M#Component]

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

case class CounterAppModel(counters: CountersModel = CountersModel(), counterStore: CounterStoreModel = CounterStoreModel())

object CounterApp extends App {

  import scala.util.{Try, Success, Failure}

  type C = Counters
  type M = CounterAppModel

  val initialState = CounterAppModel()

  val fxPanel = new JFXPanel()

  var _component: C = _

  val context: Context[M] = new Context[M] {

    lazy val name = s"DSP-MainComponent"
    println(s"[$name] created ")

    override val changes = BehaviorSubject[M]
    override val chain: UpdateChain[M] = UpdateChain()
    override val channel = Subject[Action]

    override val chainExecutor = ExecutionContext.fromExecutor(null: Executor)
    override val changesExecutor = JavaFXScheduler.executor

    lazy val chainScheduler = new ScalaScheduler {
      val asJavaScheduler = Schedulers.from(chainExecutor)
    }

    lazy val changesScheduler = new ScalaScheduler {
      val asJavaScheduler = Schedulers.from(changesExecutor)
    }


    val stream = channel.subscribeOn(chainScheduler).scan(initialState) { (oldState, action) =>
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

    stream.drop(1).subscribeOn(changesScheduler).subscribe ({ newState =>
      println(s"[$name] - A publishing a change: $newState")
      changes.onNext(newState)
    })
  }


  Repository.services("counterStore") = new CounterStore(context.map(GenLens[CounterAppModel](_.counterStore)), initialState.counterStore)

  Platform.runLater(new Runnable() {
    override def run(): Unit = {

      JavaFXModule[CountersController](context.map(GenLens[CounterAppModel](_.counters)), initialState.counters) match {
        case Success((parent, appController, appComponent)) =>
          _component = appComponent
          val stage = new Stage
          stage.setScene(new Scene(parent))
          stage.setTitle("CounterPair App")
          stage.show()
        case Failure(e) =>
          e.printStackTrace()
      }

    }
  })

  object Repository {
    private[CounterApp] val services = scala.collection.mutable.HashMap.empty[String, BaseComponent]

    def service[A : Manifest]: A = {
      val mf = manifest[A]
      val service = services.find { case(name, service) =>
        mf.runtimeClass.isInstance(service)
      }
      if(service.isEmpty) throw new IllegalStateException(s"Service not found by type: ${mf.runtimeClass.getName}")

      service.asInstanceOf[A]
    }
  }
}
