package checker

class Interval(var dom: Set[Int]) {

  def min: Int = domain.min
  def max: Int = domain.max
  var pos: Int = min

  def position: Int = pos

  def incrementPos(): Unit = pos = pos + 1

  def resetPos(): Unit = pos = min

  def posInInterval: Boolean = pos <= max

  def domain: Set[Int] = dom

  def getRange: Range = min to max

  def removeMin(): Unit = {
    if (domain.size == 1) throw new NoSolutionException
    dom = domain - min
  }

  def removeMax(): Unit = {
    if (domain.size == 1) throw new NoSolutionException
    dom = domain - max
  }

  def remove(i : Int): Unit = {
    if (domain.size == 1) throw new NoSolutionException
    dom = domain - i
  }

  def remove(minOrMax: Boolean): Unit = {
    if (minOrMax) removeMin()
    else removeMax()
  }

  def giveValue(minOrMax: Boolean): Int = {
    if (minOrMax) min
    else max
  }

}