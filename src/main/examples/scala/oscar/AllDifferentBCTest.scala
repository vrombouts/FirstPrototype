package oscar

import checker.{NoSolutionException, _}
import CPChecker._
import checker.prunings.BoundZPruning
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffBC
import oscar.cp.core.CPPropagStrength

/*
 * Testing OscaR's bound(Z) consistent filtering for the allDifferent constraint.
 * Here, we will test that the algorithm achieves bound(Z) consistency.
 */
object AllDifferentBCTest extends App {

  // creation of the trusted filtering algorithm
  val bugFree: Filter = new BoundZPruning(allDifferentChecker _)

  // creation of the OscaR filtering algorithm to be tested
  val tested: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
      allDiffBCFiltering(variables)
    }
  }

  // setting the test parameters
  testArguments.setRangeForAll(-5, 5)

  // checking that both filtering return the same filtered domains over random test instances
  CPChecker.check(bugFree, tested)


  /*
   * OscaR's bound(Z) consistent filtering algorithm for the allDifferent constraint
   */
  private def allDiffBCFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad = new AllDiffBC(variables)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }

  def allDifferentChecker(x: Array[Int]): Boolean = x.toSet.size == x.length

}
