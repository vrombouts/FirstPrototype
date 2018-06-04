package oscar

import checker.{NoSolutionException, _}
import CPChecker._
import checker.filterings.BoundZFiltering
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.core.CPPropagStrength

/*
 * Testing OscaR's Bound(Z) consistent filtering for the constraint sum(x)>15
 * Here, we will test that the filtering reached Bound(Z) consistency
 */
object SumBCTest extends App {

  // OscaR's filtering to be tested
  val testedFiltering: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = sumGTFiltering(variables)
  }

  // a trusted bound(Z) filtering algorithm for the comparison
  val trustedFiltering: Filter = new BoundZFiltering(sumChecker _)

  // checking that the filtered domains returned by both algorithms are the same
  // i.e. checking that OscaR's filtering reaches bound(Z) consistency
  CPChecker.check(trustedFiltering, testedFiltering)


  // OscaR's filtering algorithm for the sum constraint
  private def sumGTFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad = sum(variables) > 15
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }

  def sumChecker(sol: Array[Int]): Boolean = {
    if (sol.sum > 15) true
    else false
  }

}
