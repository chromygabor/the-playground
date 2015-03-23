package metro

/**
 * Created by cry on 2015.02.15..
 */

import akka.actor.{Actor, FSM, Props}
import scala.util.Random

sealed trait TrainState
case object Idle extends TrainState
case object OnTheMove extends TrainState
case object Stopped extends TrainState
case object Open extends TrainState

case class TrainData(position: Position, speed: Int = 0, lastTick: Long = 0)

case object Start
case object Tick
//case object StartFrom(pos: Position)

object MetroTrain {
  def props(tunnel: SubwayTunnel, position: Position) = Props(new MetroTrain(tunnel, position))
}

class MetroTrain(tunnel: SubwayTunnel, position: Position) extends FSM[TrainState, TrainData] {
  startWith(Idle, TrainData(position))

  when(Idle) {
    case Event(Start, data) => goto(OnTheMove) using data.copy(speed = 100, lastTick = System.currentTimeMillis())
    case Event(Tick, _) => stay
  }

  when(OnTheMove) {
    case Event(Tick, data @ TrainData(position, speed, lastTick)) =>
      //Compute the distance went from the last tick
      val currTime = System.currentTimeMillis()
      val distance = computeDistance((currTime - lastTick), speed)



      val nextData = data.copy(position = position.addDistance(distance), lastTick = currTime)
      println(s"${nextData.position}, nextStop is: ${nextData.position.nextStopDistance}m")

      if(nextData.position.nextStopDistance < distance || (!nextData.position.hasMoreSections && nextData.position.currentSection.distance - nextData.position.distance < distance))
        goto(Stopped) using nextData
      else
        goto(OnTheMove) using nextData

  }

  when(Stopped) {
    case Event(Tick, data @ TrainData(position, speed, lastTick)) =>
      if(position.hasMoreSections == false)
        goto(Idle) using data.copy(lastTick = System.currentTimeMillis())
      else if(Random.nextInt(100) > 90)
        goto(OnTheMove) using data.copy(lastTick = System.currentTimeMillis())
      else {
        print(".")
        stay
      }
  }


  onTransition {
    case Idle -> OnTheMove => stateData match {
      case TrainData(e: Position, _, _) => println("Train has started from: " + e)
    }
    case OnTheMove -> Stopped => nextStateData match {
      case TrainData(e: Position, _, _) => print("Train has stopped at: " + e)
    }
    case Stopped -> OnTheMove => nextStateData match {
      case TrainData(e: Position, _, _) => println("\nTrain has started from a stop: " + e)
    }

    case Stopped -> Idle =>  nextStateData match {
      case TrainData(e: Position, _, _) => println("\nTrain has started to be idle because no more section: " + e)
    }
  }

  private def computeDistance(timeDiff: Long, speed: Int) = {
    (speed / 3.6) * timeDiff / 1000
  }

  initialize()
}
