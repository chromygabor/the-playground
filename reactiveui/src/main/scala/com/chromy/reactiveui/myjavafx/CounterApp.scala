package com.chromy.reactiveui.myjavafx

import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui.Atom

/**
 * Created by cry on 2015.05.24..
 */

trait Module[C] {
  def apply(): (Parent, C) = {
    val clazzName = if (getClass.getSimpleName.endsWith("$")) getClass.getSimpleName.dropRight(1) else getClass.getSimpleName
    val loader = new FXMLLoader(getClass().getResource(s"$clazzName.fxml"))
    (loader.load(), loader.getController[C])
  }
}

object CounterApp extends App {
  val fxPanel = new JFXPanel()

    Platform.runLater(new Runnable() {
      override def run(): Unit = {

        val loader = new FXMLLoader(getClass().getResource(s"CounterPair.fxml"))
        //loader.setRoot(loader)
        val view: Parent = loader.load()

        val controller = loader.getController[CounterPair]
        val root = Atom(CounterPairModel()) {
          case (oldModel, newModel) =>
            println(s"$oldModel => $newModel")
        }
        controller.dispatch(root)

        root.fire(CounterPairModel())

//        val (view, controller) = CounterPair()
        val stage = new Stage

        stage.setScene(new Scene(view))
        stage.setTitle("CounterPair App")
        stage.show()


        Platform.runLater(new Runnable() {
          override def run(): Unit = {
            val controller = loader.getController[CounterPair]
            controller.getChildren
          }
        })
      }
    })
}
