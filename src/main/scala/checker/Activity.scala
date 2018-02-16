package checker

/**
 * Activity of scheduling problems. An activity contains an earliest starting time,
 * a latest completion time and a duration
  * @author
 */
class Activity(start: Set[Int], duration: Set[Int], end:Set[Int]) {
  def this() = this(Set.empty, Set.empty, Set.empty)

  def est: Int = start.min
  def lct: Int = end.max
  def lst: Int = end.max  - duration.min
  def ect: Int = start.min+ duration.min
  def dur: Int = duration.min

}
