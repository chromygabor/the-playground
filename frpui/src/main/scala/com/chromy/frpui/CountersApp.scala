package com.chromy.frpui.javafxapp

import javafx.embed.swing.JFXPanel
import javafx.scene.Scene
import javafx.stage.Stage

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.{JavaFX, JavaFXScheduler}
import rx.lang.scala.{Scheduler => ScalaScheduler}

import scala.util.{Failure, Success}


object CountersApp extends App {
  new JFXPanel()

  lazy val services = Map[Class[_], ServiceBuilder[_]](
    classOf[CounterService] -> ServiceBuilder(uid => CounterServiceImpl(uid = Uid(uid)))
  )

  val initialState = Counters()
  val app = JavaFxApp(state = initialState, services = services, sideEffectScheduler = JavaFXScheduler()) { (context, render, initialState) =>
    SideEffect {
      JavaFX[CountersController](context, render, initialState) match {
        case Success((parent, _, effect)) => //We don't need to hold reference to controller, because the parent holds it
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
  app.offerHistory(List(
    AddCounter(Uid("50935")),
    IncrementCounter(Uid("50935")),
    IncrementCounter(Uid("50935")),
    IncrementCounter(Uid("50935")),
    IncrementCounter(Uid("50935")),
    IncrementCounter(Uid("50935")),
    IncrementCounter(Uid("50935")),
    IncrementCounter(Uid("50935")),
    AddCounter(Uid("22038")),
    IncrementCounter(Uid("22038")),
    IncrementCounter(Uid("22038")),
    IncrementCounter(Uid("22038")),
    IncrementCounter(Uid("50935"))
  ))
  
}
