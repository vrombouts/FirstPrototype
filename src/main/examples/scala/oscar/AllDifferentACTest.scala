package oscar

import checker.{NoSolutionException, _}
import checker.CPChecker._
import checker.prunings.ArcPruning
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffAC
import oscar.cp.core.CPPropagStrength

/*
 * Testing OscaR's arc consistent filtering for the allDifferent constraint.
 * Here, we will test that the algorithm achieves arc consistency.
 */
object AllDifferentACTest extends App {

  // creation of the trusted filtering algorithm
  val CPCheckerAllDifferentAC: Filter = new ArcPruning(Checkers.allDifferent())

  // creation of the OscaR filtering algorithm to be tested
  val oscarAllDifferentAC: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
      filteringAllDifferentAC(variables)
    }
  }

  // setting the test parameters
  testArguments.setRangeForAll(-5, 5)
  stats.setFolderName("allDifferentAC")

  // checking that both algorithms return the same filtered domains over random instances
  check(CPCheckerAllDifferentAC, oscarAllDifferentAC)


  /*
   * OscaR's arc consistent algorithm for the allDifferent constraint
   */
  def filteringAllDifferentAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad = new AllDiffAC(variables)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }
}
