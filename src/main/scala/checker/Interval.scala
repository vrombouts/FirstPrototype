package checker

class Interval(var dom: Set[Int]) {

  def min: Int = dom.min

  def max: Int = dom.max

  def getRange: Range = min to max

  def remove(i: Int): Unit = {
    if (dom.size == 1) throw new NoSolutionException
    dom = dom - i
  }
}