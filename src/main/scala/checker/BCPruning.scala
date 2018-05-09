package checker

import java.util.function.Function

import Conversions.checkerToScalaFunction

class BCPruning(checker: Array[Int] => Boolean) extends Filter {

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val intervals = variables.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
    changeBounds(intervals)
    intervals.map(x => x.dom)
  }

  private[this] def changeBounds(intervals: Array[Interval]): Unit = {
    var changed: Boolean = false
    for (i <- intervals.indices) {
      Array(intervals(i).min, intervals(i).max).foreach { value =>
        if (!findASolution(intervals, i, value)) {
          intervals(i).remove(value)
          changed = true
        }
      }
    }
    if (changed) changeBounds(intervals)
  }

  //Generation of all the solutions to find a solution accepted by `checker´
  private[this] def findASolution(intervals: Array[Interval], index: Int, value: Int): Boolean = {
    val currentSol: Array[Int] = Array.fill(intervals.length)(0)
    currentSol(index) = value

    def setIthVariable(currentIndex: Int): Boolean = {
      if (currentIndex == index) return setIthVariable(currentIndex + 1)
      if (currentIndex == intervals.length) return checker(currentSol)
      for (i <- intervals(currentIndex).getRange) {
        currentSol(currentIndex) = i
        if(checker(currentSol.take(currentIndex+1)))
          if(setIthVariable(currentIndex + 1))
            return true
      }
      false
    }

    setIthVariable(0)
  }


}
