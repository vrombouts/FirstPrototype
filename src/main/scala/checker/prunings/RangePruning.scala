package checker.prunings

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker.{Filter, Interval, NoSolutionException}

/**
  * This class implements the Filter abstract class to represent
  * a range filtering algorithm. The constraint of the filtering
  * algorithm is defined by the 'checker' function given to its
  * constructor.
  *
  * @param checker : a boolean function taking an instantiation
  *                or partial instantiation as argument. It sh-
  *                ould return true if the given instantiation
  *                respects the constraint it defines. if it r-
  *                eturns false for a partial instantiation 'i'
  *                , it should return false for all the instan-
  *                tiations and partial instantiations contain-
  *                ing 'i'.
  */
class RangePruning(checker: Array[Int] => Boolean) extends Filter {
  //java constructor
  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  /**
    * @param variables : array of domains
    * @return the filtered domains respecting the range consistency
    *         according to the 'checker' function
    */
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

  /**
    * @param intervals : array of the intervals
    * @param index     : index representing the position of 'value' in a solution.
    * @param value     : fixed value of a solution.
    * @return true if there exists a solution from the intervals respecting the 'checker' containing the value 'value
    *         at its 'index'th position. false otherwise.
    */
  private[this] def findASolution(intervals: Array[Interval], index: Int, value: Int): Boolean = {
    val currentSol: Array[Int] = Array.fill(intervals.length)(0)
    //set the fixed value
    currentSol(index) = value

    def setIthVariable(currentIndex: Int): Boolean = {
      //skip the fixed value
      if (currentIndex == index) return setIthVariable(currentIndex + 1)
      //at the end, check if the current instantiation is a solution
      if (currentIndex == intervals.length) return checker(currentSol)
      for (i <- intervals(currentIndex).getRange) {
        currentSol(currentIndex) = i
        //while computing instantiations, the partial instantiations are
        //checked with the 'checker'.
        if (checker(currentSol.take(currentIndex + 1)))
          if (setIthVariable(currentIndex + 1))
            return true
      }
      false
    }

    setIthVariable(0)
  }
}

