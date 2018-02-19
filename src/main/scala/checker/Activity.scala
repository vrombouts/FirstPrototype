package checker

/**
 * Activity of scheduling problems. An activity contains an earliest starting time,
 * a latest completion time and a duration
  * @author Aurelie Massart & Valentin Rombouts
 */
class Activity(var start: Set[Int],var duration: Set[Int], var end:Set[Int]) {
  def this() = this(Set.empty, Set.empty, Set.empty)

  def est: Int = start.min
  def lct: Int = end.max
  def lst: Int = end.max  - duration.min
  def ect: Int = start.min+ duration.min
  def dur: Int = duration.min


  def lctReduce(maxValue: Int): Unit = {
    end = end.filter(_ <= maxValue)
  }
  def estReduce(minValue: Int): Unit = {
    start = start.filter(_ >= minValue)
  }
}
