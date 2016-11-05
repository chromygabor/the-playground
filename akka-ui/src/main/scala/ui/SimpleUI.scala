package ui

import javafx.application.Platform
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.Scene
import javafx.scene.control.{Button, Label}
import javafx.stage.{Stage, WindowEvent}

import akka.actor._
import akka.util.Timeout
import ui.CounterService.{DecrementCounter, IncrementCounter}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Created by chrogab on 2016.09.13..
  */

case object Init extends Command

case object Initialized extends PersistentEvent

/**
  * SimpleUI Behavior
  */

object CounterService {
  case object AddCounter
  case class IncrementCounter(behaviorId: BehaviorId)
  case class DecrementCounter(behaviorId: BehaviorId)
  case class CloseCounter(behaviorId: BehaviorId)
}

class CounterService extends Actor {
  override def receive = loop(Map())

  def loop(counters: Map[BehaviorId, Int]): Receive = {
    case AddCounter =>
      val newBehaviorId = BehaviorId()
      val initialValue = 0
      val newCounters = counters.updated(newBehaviorId, initialValue)
      Thread.sleep(1000)
      sender() ! newBehaviorId -> initialValue
      context.become(loop(newCounters))
    case IncrementCounter(behaviorId) =>
      val newCounters = counters.updated(behaviorId, counters.getOrElse(behaviorId, 0) + 1)
      Thread.sleep(1000)
      sender() ! newCounters(behaviorId)
      context.become(loop(newCounters))
    case DecrementCounter(behaviorId) =>
      val newCounters = counters.updated(behaviorId, counters.getOrElse(behaviorId, 0) -1)
      Thread.sleep(1000)
      sender() ! newCounters(behaviorId)
      context.become(loop(newCounters))
  }

}

object SimpleUI extends App {
  val system = ActorSystem("UI-system")

  val eventStream = system.actorOf(Props(new EventStreamActor), s"EventStreamActor")

  implicit val backgroundExecutor = scala.concurrent.ExecutionContext.global
  val sideEffectExecutor = ExecutionContext.fromExecutor(JavaFXExecutor)

  system.actorOf(Props(new Actor {
    val appContext = new AppContext(context, self, eventStream, backgroundExecutor, Timeout(5.seconds))
    val behaviorContext = new BehaviorContext(appContext)
    val controllerContext = new ControllerContext(behaviorContext)

    /**
      * Creating services
      */
    appContext.create(ServiceId("CounterService"), Props(new CounterService))

    behaviorContext.create[CountersBehavior]().onComplete {
      case Success(behaviorId) =>
        val parent = controllerContext.load[CountersController](behaviorId)
        Platform.runLater(new Runnable {
          override def run(): Unit = {
            val stage = new Stage
            stage.setScene(new Scene(parent))
            stage.setTitle("CounterPair App")
            stage.show()
            stage.setOnCloseRequest(new EventHandler[WindowEvent] {
              override def handle(event: WindowEvent): Unit = {
                context.system.terminate()
              }
            })

            context.system.whenTerminated.onComplete {
              case _ =>
                Platform.runLater(new Runnable {
                  override def run(): Unit = {
                    stage.close()
                  }
                })
            }

          }
        })
      case Failure(error) =>
        error.printStackTrace()
    }



    override def receive: Actor.Receive = {
      case e => println(s"$e received")
    }
  }), s"MainActor")

}



