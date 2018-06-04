package checker.prunings

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker.{Filter, NoSolutionException}

/**
  * This class implements the Filter abstract class to represent
  * an arc consistent filtering algorithm. The constraint of the
  * filtering algorithm is defined by the 'checker' function gi-
  * ven to its constructor.
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
class ArcPruning(checker: Array[Int] => Boolean) extends Filter {
  //java constructor
  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  /**
    * @param variables : array of domains
    * @return the filtered domains respecting the arc/domain consistency
    *         according to the 'checker' function
    */
  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if (variables.isEmpty) throw new NoSolutionException
    val solutions: Set[Array[Int]] = variables.foldLeft(Set[Array[Int]](Array()))((acc: Set[Array[Int]], x: Set[Int]) => {
      var sol: Set[Array[Int]] = Set[Array[Int]]()
      x.foreach(elem => acc.foreach(y => sol += y :+ elem))
      sol.filter(x => checker(x))
    })
    if (solutions.isEmpty) throw new NoSolutionException
    toDomains(solutions)
  }

  /**
    * @param solutions : set of solutions. All solutions must have the same length
    * @return the arc consistent domains according to those solutions
    */
  private[this] def toDomains(solutions: Set[Array[Int]]): Array[Set[Int]] = {
    val variables: Array[Set[Int]] = Array.fill(solutions.head.length)(Set.empty)
    solutions.foreach { sol =>
      for (i <- variables.indices)
        variables(i) += sol(i)
    }
    variables
  }
}
