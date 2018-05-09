package checker

class Interval(var dom: Set[Int]) {

  def min: Int = dom.min
  def max: Int = dom.max
  var pos: Int = min

  def position: Int = pos

  def incrementPos(): Unit = pos = pos + 1

  def resetPos(): Unit = pos = min

  def posInInterval: Boolean = pos <= max

  def getRange: Range = min to max

  def removeMin(): Unit = {
    if (dom.size == 1) throw new NoSolutionException
    dom = dom - min
  }

  def removeMax(): Unit = {
    if (dom.size == 1) throw new NoSolutionException
    dom = dom - max
  }

  def remove(i : Int): Unit = {
    if (dom.size == 1) throw new NoSolutionException
    dom = dom - i
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