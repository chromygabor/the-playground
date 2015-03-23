package metro

/**
 * Created by cry on 2015.02.09..
 */
case class Position(sections: List[ISection], distance: Double = 0) {

  /**
   * contains the current section
   */
  lazy val currentSection = sections.head

  /**
   * Returns a new Position which is far from the this Position with the distance in meters
   */
  def addDistance(distanceToAdd: Double): Position = {
    currentSection.distance - distance match {
      case remainder if (remainder >= distanceToAdd) => this.copy(sections, distance + distanceToAdd)
      case remainder if (remainder < distanceToAdd) =>
        val newPosition = this.copy(sections.tail, 0)
        newPosition.addDistance(distanceToAdd - remainder)
    }
  }

  /**
   * Returns that the tunnel has a next section
   */
  def hasMoreSections = !sections.tail.isEmpty

  /**
   * Contains the distance of the next stop
   */
  lazy val nextStopDistance = calculateNextStopDistance
  private def calculateNextStopDistance: Double = {
    if (currentSection.isStop && distance == currentSection.distance) 0
    else {
      val ls = sections.span { !_.isStop }
      val summedDistance = (ls._1 ++ (ls._2.head :: Nil)).foldLeft(0.0) { (accu, current) =>
        accu + current.distance
      }
      summedDistance - distance
    }
  }

  override def toString() = {
    s"$currentSection@$distance"
  }
}
