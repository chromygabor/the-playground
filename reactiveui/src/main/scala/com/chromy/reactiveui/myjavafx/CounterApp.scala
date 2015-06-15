package com.chromy.reactiveui.myjavafx

import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Scene, Parent}
import javafx.stage.Stage

import com.chromy.reactiveui.Dispatcher
import monocle.macros.GenLens
import rx.lang.scala.{Subject, Observable, Observer}

/**
 * Created by chrogab on 2015.06.04..
 */
trait Action
trait LocalAction extends Action
object CounterApp extends App {
  val fxPanel = new JFXPanel()

  case class AppModel(model: CountersModel = CountersModel())
  case object Nop extends Action

  Platform.runLater(new Runnable() {
    override def run(): Unit = {
      val loader = new FXMLLoader(getClass().getResource(s"Counters.fxml"))
      val view: Parent = loader.load()

      val root = Dispatcher[AppModel, Action]

      val actions = Subject[Action]
      val changes = Subject[CountersModel]

      val initModel = AppModel()
      val stream = actions.scan(initModel) { (oldState, action) =>
        val newState = root.update(action).run(oldState)._1
        changes.onNext(newState.model)
        newState
      }

      changes.subscribe({in => println("changes: " + in)})
      stream.subscribe({ in => })

      val controller = loader.getController[Counters]
      controller.dispatch(root.fromLens(GenLens[AppModel](_.model)), actions, changes.distinctUntilChanged)

      actions.onNext(Nop)
      val stage = new Stage
      stage.setScene(new Scene(view))
      stage.setTitle("CounterPair App")
      stage.show()

    }
  })

}