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

    def setIthVariable(currentIndex: Int): Boolean = {
      if (currentIndex == index) return setIthVariable(currentIndex + 1)
      if (currentIndex == intervals.length) return checker(currentSol)
      for (i <- intervals(currentIndex).getRange) {
        currentSol(currentIndex) = i
        if (setIthVariable(currentIndex + 1))
          return true
      }
      false
    }

    setIthVariable(0)
  }
}
