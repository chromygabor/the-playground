package com.chromy.frpui.javafxapp

import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.scene.Scene
import javafx.stage.Stage

import com.chromy.frpui.{Renderer, ConfigServiceImpl}
import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.{Controller, JavaFX, JavaFXScheduler}
import rx.lang.scala.{Scheduler => ScalaScheduler}

import scala.util.{Failure, Success}




object CountersApp extends App {
  new JFXPanel()

  lazy val services = Map[Class[_], ServiceBuilder[_]](
    classOf[CounterService] -> ServiceBuilder.singleton(CounterServiceImpl())
  )

  val initialState = Counters()
  val app = FrpApp[Counters](services = services, state = initialState, sideEffectScheduler = JavaFXScheduler())

  class App extends Controller[Unit] {
    override protected val renderer = Renderer { in =>
      SideEffect {
        JavaFX[CountersController](context, renderer, app.initialState) match {
          case Success((parent, _, effect)) =>  //We don't need to hold reference to controller, because the parent holds it
            val stage = new Stage
            stage.setScene(new Scene(parent))
            stage.setTitle("CounterPair App")
            stage.show()
            effect.run()
          case Failure(e) =>
            e.printStackTrace()
        }
      }
    }
  }
  
//  Platform.runLater(new Runnable() {
//    override def run(): Unit = {
//      JavaFX[CountersController](???, app.render, app.initialState) match {
//        case Success((parent, _, effect)) =>  //We don't need to hold reference to controller, because the parent holds it
//          val stage = new Stage
//          stage.setScene(new Scene(parent))
//          stage.setTitle("CounterPair App")
//          stage.show()
//          effect.run()
//        case Failure(e) =>
//          e.printStackTrace()
//      }
//
//    }
//  })
}
