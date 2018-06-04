package checker.prunings

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker.{Filter, Interval, NoSolutionException}

/**
  * This class implements the Filter abstract class to represent
  * a bound(Z) consistent filtering algorithm. The constraint of
  * the filtering algorithm is defined by the 'checker' function
  * given to its constructor.
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
class BoundZPruning(checker: Array[Int] => Boolean) extends Filter {
  //java constructor
  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  /**
    * @param variables : array of domains
    * @return the filtered domains respecting the bound(Z) consistency
    *         according to the 'checker' function
    */
  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    //get the intervals from the variables
    val intervals = variables.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
    //apply bound consistency
    var changed: Boolean = true
    while (changed) {
      changed = false
      //for each interval, remove its min and max if they are not part of a solution
      for (i <- intervals.indices)
        if (changeBounds(i, intervals)) changed = true
    }
    intervals.map(x => x.dom)
  }

  def changeBounds(i: Int, intervals: Array[Interval]): Boolean = {
    Array(intervals(i).min, intervals(i).max).foldLeft(false) { (acc, value) =>
      if (!findASolution(intervals, i, value)) {
        intervals(i).remove(value)
        true
      } else acc
    }
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
