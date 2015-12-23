package com.chromy.frpui.javafxapp

import java.io.File
import javafx.embed.swing.JFXPanel
import javafx.scene.Scene
import javafx.stage.Stage

import com.chromy.frpui.fw.core._
import com.chromy.frpui.fw.javafx.{JavaFX, JavaFXScheduler}
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import rx.lang.scala.{Scheduler => ScalaScheduler}

import scala.util.{Failure, Success}
import scala.io.Source


object CountersApp extends App {
  new JFXPanel()

  lazy val services = Map[Class[_], ServiceBuilder[_]](
    classOf[CounterService] -> ServiceBuilder(uid => CounterServiceImpl(uid = Uid(uid)))
  )

  val file = new File("events.txt")
  val events: List[Event] = if(file.exists()) {
    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.enableDefaultTyping(ObjectMapper.DefaultTyping.NON_FINAL, JsonTypeInfo.As.WRAPPER_OBJECT);

    (for(line <- Source.fromFile(file).getLines()) yield {
      mapper.readValue[Event](line)
    }).toList
  } else  Nil
  
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
  app.offerHistory(events)
  
}
