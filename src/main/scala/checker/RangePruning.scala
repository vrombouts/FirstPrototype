package checker

import java.util.function.Function

import Conversions.checkerToScalaFunction

class RangePruning(checker: Array[Int] => Boolean) extends Filter {

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val intervals: Array[Interval] = variables.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
    filterIntervals(intervals)
    intervals.map(x => x.dom)
  }

  private[this] def filterIntervals(intervals: Array[Interval]): Unit = {
    if (intervals.indices.foldLeft(false) { (acc, x) => if (filterInterval(x, intervals)) true else acc })
      filterIntervals(intervals)
  }

  def filterInterval(index: Int, intervals: Array[Interval]): Boolean = {
    val domain: Set[Int] = intervals(index).dom
    intervals(index).dom = domain.filter(findASolution(intervals, index, _))
    if (intervals(index).dom.isEmpty) throw NoSolutionException()
    domain.size != intervals(index).dom.size
  }

  private[this] def findASolution(intervals: Array[Interval], index: Int, value: Int): Boolean = {
    val currentSol: Array[Int] = Array.fill(intervals.length)(0)
    currentSol(index) = value

    def setIthVariable(currentIndex: Int): Boolean = {
      if (currentIndex == index) return setIthVariable(currentIndex + 1)
      if (currentIndex == intervals.length) return checker(currentSol)
      for (i <- intervals(currentIndex).getRange) {
        currentSol(currentIndex) = i
        if (checker(currentSol.take(currentIndex + 1)))
          if (setIthVariable(currentIndex + 1))
            return true
      }
      false
    }

    setIthVariable(0)
  }
}

