package checker.constraints

import checker.{Filterings, NoSolutionException}

trait BCPruning extends Filterings {
  override protected[this] def applyConstraintBC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    applyBCPruning(variables)
  }


  //applying BC with pruning //
  @throws[NoSolutionException]
  def applyBCPruning(variables: Array[Set[Int]]): Array[Set[Int]] = {
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
      val modif: Boolean = cartesianBC(intervals, i, minOrMax = true)
      val other_modif: Boolean = cartesianBC(intervals, i, minOrMax = false)
      if (modif || other_modif) changed = true
    }
    changed
  }

  @throws[NoSolutionException]
  private[this] def cartesianBC(intervals: Array[Interval], id: Int, minOrMax: Boolean): Boolean = {
    reinitialize(intervals)
    val interval: Interval = intervals(id)
    var sol: Array[Int] = Array(interval.giveValue(minOrMax))
    if (intervals.length == 1) {
      if (checker(sol))
        return false
      else {
        if (interval.domain.size == 1) throw new NoSolutionException
        interval.update(minOrMax)
        return true
      }
    }
    var i: Int = 0
    while (i < intervals.length && sol.nonEmpty) {
      if (i == id) i = i + 1
      val currentInter = intervals(i)
      if (currentInter.posInInterval) {
        sol = sol :+ currentInter.position
        currentInter.incrementPos()
        if (!findingAcceptingValue(sol, currentInter)) {
          //backtrack
          sol = sol.dropRight(2)
          i = if (i == id + 1) i - 3 else i - 2
        } else if (sol.length == intervals.length)
        //one element of the cartesian product respect the constraint.
        //Therefore, no update of the interval's min/max is required
          return false
      } else {
        //backtrack
        currentInter.resetPos()
        sol = sol.dropRight(1)
        i = if (i == id + 1) i - 3 else i - 2
      }
      i = i + 1
    }
    //no solution if we remove the last element of the domain of a variable
    if (interval.domain.size == 1) throw new NoSolutionException
    interval.update(minOrMax)
    true

  }


  private[this] def reinitialize(intervals: Array[Interval]): Unit = {
    intervals.foreach(x => x.resetPos())
  }

  private[this] def findingAcceptingValue(sol: Array[Int], interval: Interval): Boolean = {
    if (checker(sol)) true
    else if (interval.posInInterval) {
      sol(sol.length - 1) = interval.position
      interval.incrementPos()
      findingAcceptingValue(sol, interval)
    } else {
      interval.resetPos()
      false
    }
  }


}
