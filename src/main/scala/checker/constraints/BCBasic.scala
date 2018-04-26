package checker.constraints

import checker.{Base, NoSolutionException}

trait BCBasic extends Base {
  override protected[this] def applyConstraintBC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    applyBC(variables)
  }

  //applying BC without pruning //
  @throws[NoSolutionException]
  def applyBC(variables: Array[Set[Int]]): Array[Set[Int]] = {
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
      val modif: Boolean = applyBCOnOneValue(intervals, i)
      val other_modif: Boolean = applyBCOnOneValue(intervals, i, isMin = false)
      if (modif || other_modif) changed = true
    }
    changed
  }

  @throws[NoSolutionException]
  private[this] def applyBCOnOneValue(intervals: Array[Interval], index: Int, isMin: Boolean = true): Boolean = {
    if (findASolution(intervals, index, isMin))
      false
    else {
      if (intervals(index).domain.size == 1) throw new NoSolutionException
      intervals(index).update(isMin)
      true
    }
  }

  //Generation of all the solutions
  private[this] def findASolution(intervals: Array[Interval], index: Int, isMin: Boolean): Boolean = {
    val currentSol: Array[Int] = Array.fill(intervals.length)(0)
    currentSol(index) = intervals(index).giveValue(isMin)
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
