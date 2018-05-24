package checker

/**
  * hybrid filtering but which can apply every type of consistencies implemented by CPChecker.
  *
  * @param filterings : array corresponding to each variable telling the consistency apply to it.
  *                   1 for AC,
  *                   2 for BC,
  *                   3 for RC.
  * @param checker    : the checker function representing the constraint.
  */

class HybridFiltering(filterings: Array[Int], checker: Array[Int] => Boolean) extends Filter {


  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val vars  = variables

    //The ac variables can directly be obtained thanks to ACFiltering.
    val ac = new ACFiltering(checker)
    val acVars = ac.filter(variables)
    for(i <- vars.indices){
      if(filterings(i)==1) vars(i) = acVars(i)
    }
    // the BC and RC variable must recursively be reduced.
    val intervals = vars.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
    filterIntervals(intervals)
    intervals.map(x => x.dom)
  }


  private[this] def filterIntervals(intervals: Array[Interval]): Unit = {
    if (intervals.indices.foldLeft(false) { (acc, i) =>
      if (filterings(i) == 2) if (filterBC(i, intervals)) true else acc
      else if (filterings(i) == 3) if (filterRC(i, intervals)) true else acc
      else acc
    })
      filterIntervals(intervals)
  }

  private[this] def filterBC(index: Int, intervals: Array[Interval]): Boolean = {
    Array(intervals(index).min, intervals(index).max).foldLeft(false) { (acc, i) =>
      if (findASolution(intervals, index, i)) {
        intervals(index).remove(i)
        true
      } else
        acc
    }
  }

  private[this] def filterRC(index: Int, intervals: Array[Interval]): Boolean = {
    intervals(index).dom.foldLeft(false) { (acc, i) =>
      if (findASolution(intervals, index, i)) {
        intervals(index).remove(i)
        true
      } else
        acc
    }
  }

  private[this] def findASolution(intervals: Array[Interval], index: Int, value: Int): Boolean = {
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
