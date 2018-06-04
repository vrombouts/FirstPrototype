package checker.prunings

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker.{Filter, NoSolutionException}

/**
  * This class implements the Filter abstract class to represent
  * a bound(D) consistent filtering algorithm. The constraint of
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
class BoundDPruning(checker: Array[Int] => Boolean) extends Filter {
  //java constructor
  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  /**
    * @param variables : array of domains
    * @return the filtered domains respecting the bound(D) consistency
    *         according to the 'checker' function
    */
  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    //apply the bound consistency
    var changed: Boolean = true
    while (changed) {
      changed = false
      //for each domain, remove its min and max if they are not part of a solution.
      for (i <- variables.indices)
        if (changeBounds(i, variables)) changed = true
    }
    variables
  }


  def changeBounds(i: Int, variables: Array[Set[Int]]): Boolean = {
    Array(variables(i).min, variables(i).max).foldLeft(false) { (acc, value) =>
      if (!findASolution(variables, i, value)) {
        if (variables(i).size == 1) throw NoSolutionException()
        variables(i) -= value
        true
      } else acc
    }
  }

  /**
    * @param variables : array of the variables' domains
    * @param index     : index representing the position of 'value' in a solution.
    * @param value     : fixed value of a solution.
    * @return true if there exists a solution from the domains respecting the 'checker' containing the value 'value
    *         at its 'index'th position. false otherwise.
    */
  private[this] def findASolution(variables: Array[Set[Int]], index: Int, value: Int): Boolean = {
    val currentSol: Array[Int] = Array.fill(variables.length)(0)
    //set the fixed value
    currentSol(index) = value

    def setIthVariable(currentIndex: Int): Boolean = {
      //skip the fixed value
      if (currentIndex == index) return setIthVariable(currentIndex + 1)
      //at the end, check if the current instantiation is a solution
      if (currentIndex == variables.length) return checker(currentSol)
      for (i <- variables(currentIndex)) {
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
