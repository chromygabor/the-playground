package dummy

/**
 * Created by cry on 2015.01.11..
 */
object AkkaTest extends App {
  val swt = new SubwayTunnel

  def startTrain(): Unit = {

  }
}

sealed trait TrainState
case class OnTheMove() extends TrainState
case object Idle extends TrainState

sealed trait TrainData
case object Unstarted extends TrainData
case class Position(section: Section, distance: Int) extends TrainData

case object Move

//class Train(timeout: FiniteDuration) extends Actor with FSM[TrainState, TrainData] {
//  startWith(Idle, Unstarted, Some(timeout))
//
//  when(Idle) {
//    case Event(Move, Unstarted) =>
//  }
//
//  initialize()
//}

class SubwayTunnel {
  val sections = List[ISection](
    StopSection("Újpest-központ", 100),
    Section(300),
    Section(300),
    Section(500),
    StopSection("Újpest-városkapu", 100),
    Section(300),
    Section(400),
    Section(300),
    Section(500),
    Section(300),
    StopSection("Gyöngyösi utca", 150),
    Section(500),
    Section(300),
    StopSection("Forgách utca", 150)
  )
}

trait ISection {
  val distance: Int
  val isStop: Boolean
}

case class StopSection(name: String, distance: Int, isStop:Boolean  = true) extends ISection
case class Section(distance: Int, isStop:Boolean  = false) extends ISection


