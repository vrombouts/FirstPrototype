package checker

import java.util.function.Function

import Conversions.checkerToScalaFunction

class BCFiltering(checker: Array[Int] => Boolean) extends Filter{

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    filterBC(variables)
  }


  //applying BC without pruning //
  @throws[NoSolutionException]
  def filterBC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val intervals = getIntervals(variables)
    while (changeBounds(intervals)) {}
    intervalsToVariables(intervals)
  }

  @throws[NoSolutionException]
  private[this] def getIntervals(variables: Array[Set[Int]]): Array[Interval] = {
    variables.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
  }

  private[this] def intervalsToVariables(intervals: Array[Interval]): Array[Set[Int]] = {
    intervals.map(x => x.domain)
  }

  @throws[NoSolutionException]
  private[this] def changeBounds(intervals: Array[Interval]): Boolean = {
    var changed: Boolean = false
    for (i <- intervals.indices) {
      val removeMin: Boolean = applyBCOnOneValue(intervals, i, intervals(i).min)
      val removeMax: Boolean = applyBCOnOneValue(intervals, i, intervals(i).max)
      if (removeMin || removeMax) changed = true
    }
    changed
  }

  @throws[NoSolutionException]
  private[this] def applyBCOnOneValue(intervals: Array[Interval], index: Int, value: Int): Boolean = {
    if (findASolution(intervals, index, value)) false
    else {
      intervals(index).remove(value)
      true
    }
  }

  //Generation of all the solutions to find a solution accepted by `checker´
  private[this] def findASolution(intervals: Array[Interval], index: Int, value: Int): Boolean = {
    val currentSol: Array[Int] = Array.fill(intervals.length)(0)
    currentSol(index) = value
    def setIthVariable(currentIndex: Int): Boolean = {
      if(currentIndex==index) return setIthVariable(currentIndex + 1)
      if(currentIndex==intervals.length) return checker(currentSol)
      for (i <- intervals(currentIndex).getRange) {
        currentSol(currentIndex) = i
        if(setIthVariable(currentIndex + 1))
          return true
      }
      false
    }

    setIthVariable(0)
  }


}
