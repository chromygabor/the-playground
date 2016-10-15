package ui

import javafx.application.Platform
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.Scene
import javafx.scene.control.{Button, Label}
import javafx.stage.{Stage, WindowEvent}

import akka.actor._
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Created by chrogab on 2016.09.13..
  */

case object Init extends Command

case object Initialized extends PersistentEvent

case object Empty extends Command

/**
  * SimpleUI Behavior
  */

case class CounterChanged(counter: Int) extends PersistentEvent
case class ProgressBar(on: Boolean) extends SimpleEvent

case object IncCounter extends Command
case object DecCounter extends Command

case class State(counter: Int)
class SimpleUIBehavior(val id: BehaviorId) extends Behavior[State] {

  case object IncProgressCounter extends Command
  case object DecProgressCounter extends Command

  var progressCounter = 0

  val initialState = State(0)

  override def onCommand(state: State): PartialFunction[Command, List[Event]] = {
    case Init =>
      log.debug("Init command received in behavior")
      List(Initialized)
    case IncProgressCounter =>
      progressCounter = progressCounter + 1
      if(progressCounter == 1) ProgressBar(true)
      else Nil
    case DecProgressCounter =>
      progressCounter = progressCounter - 1
      if(progressCounter == 0) ProgressBar(false)
      else Nil

    case IncCounter =>
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { (state, result) =>
        CounterChanged(state.counter + result) :: DecProgressCounter
      }
      IncProgressCounter
    case DecCounter => //Decrement counter by service
      onSuccess {
        //This is a service call
        Thread.sleep(1000)
        1
      } { (state, result) =>
        CounterChanged(state.counter - result) :: DecProgressCounter
      }

      IncProgressCounter
  }

  override def onEvent(state: State, context: Context): PartialFunction[Event, State] = {
    case CounterChanged(counter) =>
      log.debug(s"Counter changed to: $counter")
      state.copy(counter = counter)
    case Initialized =>
      log.debug("Initialized event received in behavior")
      state
  }

}


class SimpleUIController extends Controller {
  @FXML private var _incButton: Button = _

  def incButton = _incButton

  @FXML private var _decButton: Button = _

  def decButton = _decButton

  @FXML private var _counter: Label = _

  def counter = _counter

  def onEvent: EventReceive = {
    case Initialized => init()
    case ProgressBar(on) => ui {
        println(s"ProgressBar: $on")
    }
    case CounterChanged(newCounter) => ui {
      counter.setText(s"$newCounter")
    }
    case e => ui {
      println(s"Ez: $e")
    }
  }

  def init(): Ui = ui {
    //this runs on ui thread
    incButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        behavior ! IncCounter
      }
    })

    decButton.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        behavior ! DecCounter
      }
    })
  }

}
object CounterService {
  val behaviorId = BehaviorId("CounterService")
  case object AddCounter
}

case class CounterServiceState(counters: Map[Int, Int] = Map.empty)
class CounterService extends Actor {
  override def receive: Actor.Receive = ???
}

object SimpleUI extends App {
  val system = ActorSystem("UI-system")

  val eventStream = system.actorOf(Props(new EventStreamActor), s"EventStreamActor")

  implicit val backgroundExecutor = scala.concurrent.ExecutionContext.global
  val sideEffectExecutor = ExecutionContext.fromExecutor(JavaFXExecutor)

  system.actorOf(Props(new Actor {
    val behaviorContext = new Context(context, self, eventStream, backgroundExecutor, Timeout(5.seconds))
    val controllerContext = new ControllerContext(system, eventStream, backgroundExecutor, behaviorContext)

    /**
      * Creating services
      */
    //behaviorContext.create(CounterService.behaviorId, new CounterService)

    behaviorContext.create(BehaviorId("CountersBehavior"), Props(new CountersBehavior)).foreach { behaviorId =>
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
    }


    override def receive: Actor.Receive = {
      case e => println(s"$e received")
    }
  }), s"MainActor")

}



