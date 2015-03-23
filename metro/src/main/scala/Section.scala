package metro

/**
 * Created by cry on 2015.02.09..
 */
trait ISection {
  val distance: Double
  val isStop: Boolean
  val name: String
}

case class StopSection(name: String, distance: Double, isStop: Boolean = true) extends ISection
case class Section(name: String, distance: Double, isStop: Boolean = false) extends ISection
