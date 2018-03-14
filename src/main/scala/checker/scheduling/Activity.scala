package checker.scheduling

/**
  * Activity of scheduling problems. An activity contains an earliest starting time,
  * a latest completion time and a duration
  *
  * @author Aurelie Massart & Valentin Rombouts
  */
class Activity(var start: Set[Int], var duration: Set[Int], var end: Set[Int], var height: Int) {
  def this(start: Set[Int], duration: Set[Int], end: Set[Int]) = this(start, duration, end, 1)

  def this() = this(Set.empty, Set.empty, Set.empty)

  def est: Int = start.min

  def lct: Int = end.max

  def lst: Int = end.max - duration.min

  def ect: Int = start.min + duration.min

  def dur: Int = duration.min

  def isEmpty: Boolean = start.isEmpty || duration.isEmpty || end.isEmpty

  def lctReduce(maxValue: Int): Boolean = {
    val e = end.filter(_ <= maxValue)
    if (e.size == end.size)
      return false
    start = e
    true
  }

  def estReduce(minValue: Int): Boolean = {
    val s = start.filter(_ >= minValue)
    if (s.size == start.size)
      return false
    start = s
    true
  }

  /*
   * For now, this function enforce start+duration=end using duration.min
   */
  def enforceCohesion(): Unit = {
    start = start.filter(s => end.contains(s + dur))
    end = end.filter(e => start.contains(e - dur))
  }

  override def toString: String = {
    "Activity : \n" +
      "Starts: " + start +
      "\t duration: " + duration +
      "\t end: " + end + "\n"
  }

  override def equals(o: scala.Any): Boolean = {
    o match {
      case act: Activity =>
        act.start.equals(start) &&
          act.duration.equals(duration) &&
          act.end.equals(end)
      case _ => false
    }
  }
}
