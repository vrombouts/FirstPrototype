package checker

class RangeFiltering(checker : Array[Int] => Boolean) extends Filter {
  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val intervals: Array[Interval] = variables.map{new Interval(_)}
    while(range(intervals)){}
    intervals.map(x => x.domain)
  }

  def range(intervals: Array[Interval]): Boolean = {
    intervals.indices.foldLeft(false){(Acc, x) => if(range(x, intervals)) true else Acc}
  }

  private[this] def range(index: Int, intervals: Array[Interval]): Boolean = {
    val domain: Set[Int] = intervals(index).domain
    intervals(index).dom = domain.filter(findASolution(index, intervals, _))
    domain.size!=intervals(index).dom.size
  }

  private[this] def findASolution(index : Int, intervals: Array[Interval], value:Int): Boolean = {
    val currentSol: Array[Int] = Array.fill(intervals.length)(0)
    currentSol(index) = value
    setIthVariable(intervals, 0, currentSol, index)
  }

  private[this] def setIthVariable(intervals: Array[Interval], currentIndex: Int, currentSol: Array[Int], index: Int): Boolean = {
    val range = if (currentIndex == index) 0 until 1 else intervals(currentIndex).getRange
    for (i <- range) {
      if (currentIndex != index)
        currentSol(currentIndex) = i
      if (currentIndex == intervals.length - 1) {
        if (checker(currentSol)) {
          return true

        }
      } else {
        if(setIthVariable(intervals, currentIndex + 1, currentSol, index))
          return true
      }
    }
    false
  }
}
