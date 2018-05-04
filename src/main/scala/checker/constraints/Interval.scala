package checker.constraints

class Interval(private var dom: Set[Int]) {

  var min: Int = domain.min
  var max: Int = domain.max
  var pos: Int = min

  def position: Int = pos

  def incrementPos(): Unit = pos = pos + 1

  def resetPos(): Unit = pos = min

  def posInInterval: Boolean = pos <= max

  def domain: Set[Int] = dom

  def getRange: Range = min to max

  def update_min(): Unit = {
    dom = domain - min
    min = domain.min
  }

  def update_max(): Unit = {
    dom = domain - max
    max = domain.max
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
