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

  def update_min(): Unit = {
    if (domain.size == 1) throw new NoSolutionException
    dom = domain - min
  }

  def update_max(): Unit = {
    if (domain.size == 1) throw new NoSolutionException
    dom = domain - max
  }

  def remove(i : Int): Unit = {
    if (domain.size == 1) throw new NoSolutionException
    dom = domain - i
  }

  def update(minOrMax: Boolean): Unit = {
    if (minOrMax) update_min()
    else update_max()
  }

  def giveValue(minOrMax: Boolean): Int = {
    if (minOrMax) min
    else max
  }

}