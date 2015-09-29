package com.chromy.reactiveui.myjavafx

import java.util.concurrent.{Executor, Executors}
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.SideChain
import com.chromy.reactiveui.myjavafx.CounterNavigatorDialog
import monocle.macros.GenLens
import rx.lang.scala.schedulers.IOScheduler
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Observer, Scheduler => ScalaScheduler, Subject}
import rx.schedulers.Schedulers
import scala.collection.mutable.{HashMap => MMap, WeakHashMap => WMap}


import scala.concurrent.ExecutionContext
import scala.util.Try


/**
 * Created by chrogab on 2015.06.04..
 */

object Services {

  lazy val app = (context: ContextMapper[CountersModel], initialState: CountersModel) => JavaFXController[CountersController](context)

  lazy val dialogs = Map[Class[_ <: UiComponent], Class[_ <: JavaFXController]](
    classOf[CounterNavigatorDialog] -> classOf[CounterNavigatorDialogController],
    classOf[CounterNavigator] -> classOf[CounterNavigatorController]
  )

  lazy val services: Map[String, ServiceBuilder[_ <: BaseComponent]] = Map(
    "app" -> Singleton(CountersModel()) { (contextMapper, initialState) => new Counters(contextMapper) },
    "com.chromy.reactiveui.myjavafx.CounterStore" -> Singleton(CounterStore.Model()) { (contextMapper, initialState) => new CounterStore(contextMapper) },
    "com.chromy.reactiveui.myjavafx.DialogService" -> Singleton(DialogService.Model()) { (contextMapper, initialState) => new DialogService(contextMapper) }
  )
}

case class CounterAppModel(counters: CountersModel = CountersModel(), services: Map[String, _ <: ComponentModel] = Map())

object CounterApp extends App with ServiceAware {

  import scala.util.{Failure, Success, Try}

  type C = Counters
  type M = CounterAppModel

  val fxPanel = new JFXPanel()

  var _component: C = _

  val context: Context[M] = new Context[M] {

    lazy val name = s"DSP-MainComponent"
    println(s"[$name] created ")

    override val changes = SideChain[M]
    override val chain: UpdateChain[M] = UpdateChain()
    override val channel = Subject[Action]
    override val initialState = CounterAppModel()

    val chainSch = IOScheduler()
    val changesSch = JavaFXScheduler()
    override val backgroundExecutor: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    println(s"App initialstate: $initialState")

    val stream = channel.observeOn(chainSch).scan(initialState) { case (oldState, action) =>
      Try {
        println(s"=================== [$name] action received  $action ================= on ${Thread.currentThread.getName}")
        println(s"[$name] - An action received in the main loop: $action, updating state $oldState...")
        val newState = chain.update(action, oldState)
        println(s"[$name] - result is: $newState")
        newState
      } match {
        case Success(newState) => newState
        case Failure(error) =>
          error.printStackTrace()
          oldState
      }
    }

    stream.drop(1).observeOn(chainSch).subscribe({ newState =>
      println(s"[$name] - A publishing a change: ${newState} on: ${Thread.currentThread().getName}")
      val r = changes.update(newState)
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
           r.run()
        }
      })
    })
  }



  Platform.runLater(new Runnable() {
    override def run(): Unit = {
      JavaFXController[CountersController](context.map(GenLens[CounterAppModel](_.counters))) match {
        case Success((parent, appController, appComponent)) =>
          _component = appComponent
          val stage = new Stage
          stage.setScene(new Scene(parent))
          stage.setTitle("CounterPair App")
          stage.show()
          context.channel.onNext(Nop)
        case Failure(e) =>
          e.printStackTrace()
      }

    }
  })
  override protected val servicesContextMapper: ContextMapper[Map[String, _ <: ComponentModel]] = context.map(GenLens[CounterAppModel](_.services))
  override protected val serviceDefs: Map[String, ServiceBuilder[_ <: BaseComponent]] = Services.services
}
