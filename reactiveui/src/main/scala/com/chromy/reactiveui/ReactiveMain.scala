package com.chromy.reactiveui

import java.awt.Toolkit
import java.util.UUID
import java.util.function.{Consumer, Predicate}
import javafx.application.{Platform, Application}
import javafx.collections.ObservableList
import javafx.embed.swing.JFXPanel
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Node, Scene}
import javafx.stage.Stage
import rx.lang.scala.{Subject, Observable}

/**
 * Created by cry on 2015.04.17..
 */
trait Action

case class WrappedAction(action: Action, componentId: ComponentId) extends Action

object Action {
  def apply(action: Action, componentId: ComponentId): WrappedAction = WrappedAction(action, componentId)

  def unapply(action: Action): Option[(Action, ComponentId)] = action match {
    case WrappedAction(action: Action, componentId: ComponentId) => Some((action, componentId))
    case _ => None
  }
}

case class ComponentId(id: String)

trait Component[T] {
  this: {
    def getId(): String
    def getChildren(): ObservableList[Node]
  }  =>

  private val loader = new FXMLLoader(getClass().getResource(getClass.getSimpleName + ".fxml"));
  loader.setRoot(this)
  loader.setController(this)
  loader.load()

  val id: ComponentId = ComponentId(if (this.getId == null) getClass.getSimpleName + "#" + UUID.randomUUID().toString else this.getId)

  val subject = Subject[Action]()
  protected def send(action: Action) = {
    //println("Action was sent: " + action)
    subject.onNext(Action(action, id))
  }

  def update(action: Action, model: T): T
  def render(model: T)

  getChildren.filtered(new Predicate[Node] {
    override def test(t: Node): Boolean = t.isInstanceOf[Component[_]]
  }).forEach(new Consumer[Node] {
    override def accept(t: Node): Unit = {
      t.asInstanceOf[Component[_]].subject.subscribe{ (action) =>
        send(action)
      }
    }
  })
}

object Component {
  implicit def componentToObservable(component: Component[_]) = component.subject.asInstanceOf[Observable[Action]]
}

object Utils {
  implicit def func2eventHandler(in: () => Unit): EventHandler[ActionEvent] = new EventHandler[ActionEvent] {
    override def handle(event: ActionEvent): Unit = in()
  }
}

object ReactiveMain extends App {

  val fxPanel = new JFXPanel()

  Platform.runLater(new Runnable() {
    override def run(): Unit = {
      val stage = new Stage
      val appFrame = new AppFrame
      val scene = new Scene(appFrame)

      val model0 = AppFrameModel()
      appFrame.render(model0)

      appFrame.foldLeft(model0) { (prevModel, action) =>
        action match {
          case Action(action, appFrame.id) =>
            val newModel = appFrame.update(action, prevModel)
            appFrame.render(newModel)
            newModel
        }
      }.subscribe(
      {(action) => println("Action was fired: action")},
      {(error) => error.printStackTrace()},
      {() => }
      )


      stage.setScene(scene)
      stage.setTitle("ReactiveUI")
      stage.centerOnScreen()
      stage.show()
    }
  })
}
