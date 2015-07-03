package com.chromy.reactiveui.myjavafx

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui.Dispatcher
import com.chromy.reactiveui.Dispatcher.DispatcherFactory
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Observable, Observer, Scheduler => ScalaScheduler, Subject}
import rx.schedulers.Schedulers

import scala.util.{Failure, Success, Try}

/**
 * Created by chrogab on 2015.06.04..
 */
trait Action

trait LocalAction extends Action

trait ActionWrapper extends Action {
  val action: Action
}

object Uid {
  private var _id: Int = 0

  def nextUid() = {
    val res = _id
    _id = _id + 1
    res
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

sealed trait Module {
  type Model
  type Dispatcher
}

trait GenericModule[M, D] extends Module {
  type Model = M
  type Dispatcher = D
}

sealed trait JavaFXModule {
  type Controller
  type Model
  type Dispatcher

  def dispatch(parentFactory: DispatcherFactory[Model, Action], actions: Observer[Action], changes: Observable[Model], initialState: Model): Dispatcher
}

trait GenericJavaFXModule[A <: Module] extends JavaFXModule {
  type Controller = this.type
  type Model = A#Model
  type Dispatcher = A#Dispatcher
}

object JavaFXFactory {
  def apply[T <: JavaFXModule : Manifest](parentFactory: DispatcherFactory[T#Model, Action], actions: Observer[Action], changes: Observable[T#Model], initialState: T#Model): (Parent, T, T#Dispatcher) = {

    val ct = manifest[T]

    val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
    val loader = new FXMLLoader(getClass().getResource(s"$clazzName.fxml"))

    val node: Parent = loader.load()
    val controller = loader.getController[T]
    val dispatcher = controller.dispatch(parentFactory.asInstanceOf[DispatcherFactory[controller.Model, Action]], actions, changes.asInstanceOf[Observable[controller.Model]], initialState.asInstanceOf[controller.Model])
    (node, controller, dispatcher.asInstanceOf[T#Dispatcher])
  }
}


object CounterApp extends App {
  val fxPanel = new JFXPanel()

  case class AppModel(model: CountersModel = CountersModel())

  case object Nop extends Action

  Platform.runLater(new Runnable() {
    override def run(): Unit = {

      val root = Dispatcher[CountersModel, Action]

      val actions = Subject[Action]
      val changes = Subject[CountersModel]

      val initModel = CountersModel()
      val stream = actions.observeOn(ComputationScheduler()).scan(initModel) { (oldState, action) =>
        Try {
          val newState = root.update(action).run(oldState)._1
          println(s"[DSP-Main(0)] - An action received in the main loop: $action -- $oldState => $newState")
          newState
        } match {
          case Success(newState) => newState
          case Failure(error) =>
            error.printStackTrace()
            oldState
        }
      }

      //actions.subscribe({ in => println(s"[DSP-Main(0)] - An action received in the main loop: $in") })
//      changes.subscribe({ in => println(s"[DSP-MAIN] - A change is published from main loop: $in\n======================") })
      stream.subscribe({ newState =>
        println(s"[DSP-Main(0)] - A publishing a change: $newState")
        changes.onNext(newState)
      })

      val (appComponent, appController, appDispatcher) = JavaFXFactory[Counters](root.factory, actions, changes.observeOn(JavaFXScheduler()).distinctUntilChanged, initModel)
      actions.onNext(Nop)

      val stage = new Stage
      stage.setScene(new Scene(appComponent))
      stage.setTitle("CounterPair App")
      stage.show()
    }
  })

}
