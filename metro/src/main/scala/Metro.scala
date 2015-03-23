package metro

/**
 * Created by cry on 2015.02.09..
 */
object Metro extends App {
  val tunnel = new SubwayTunnel

  val section = tunnel.sections.last

  val newState = addVehicle(tunnel.state, section)
  println(newState.mkString("\n"))
  println("============")
  val newState2 = addVehicle(newState, tunnel.sections.tail.tail.tail.head)
  println(newState2.mkString("\n"))

  def addVehicle(state: List[TubeSectionState], section: ISection): List[TubeSectionState] = {
    val i = state.indexWhere(_.section == section)

    val newState = state.updated(i, state(i).copy(hasVehicle = true)).toVector

    val newStates = for {
      index <- newState.size-1 to 0 by -1
    } yield {
      val isNextSectionFull = (newState.size > index + 1) && newState(index+1).hasVehicle
      val isNextNextSectionFull = (newState.size > index + 2) && newState(index+2).hasVehicle
      newState(index).copy(hasNextVehicle = isNextSectionFull, hasNextNextVehicle = !isNextSectionFull && isNextNextSectionFull)
    }
    newStates.reverse.toList
  }
}

case class TubeSectionState(section: ISection, hasVehicle: Boolean, hasNextVehicle: Boolean, hasNextNextVehicle: Boolean)

class SubwayTunnel {
  val sections = List[ISection](
    StopSection("Újpest-központ Depot", 100),
    Section("0-0", 100),
    StopSection("Újpest-központ", 100),
    Section("1-0", 300),
    //    Section("1-1", 300),
    //    Section("1-2", 500),
    //    StopSection("Újpest-városkapu", 100),
    //    Section("2-0", 300),
    //    Section("2-1", 400),
    //    Section("2-2", 300),
    //    Section("2-3", 500),
    //    Section("2-4", 300),
    //    StopSection("Gyöngyösi utca", 150),
    //    Section("3-0", 500),
    //    Section("3-1", 300),
    StopSection("Forgách utca", 150))

  val state = sections.map { s => TubeSectionState(s, false, false, false)}
  val FirstPosition = Position(sections, sections.head.distance)
}