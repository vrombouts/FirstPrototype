package oscar

import checker.{NoSolutionException, _}
import CPChecker._
import checker.filterings.ArcFiltering
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.GCCVarAC
import oscar.cp.core.CPPropagStrength

/*
 * Testing OscaR's filtering for the GCC constraint.
 * Here, we will test that the filtering reaches arc consistency
 */
object GCCACTest extends App {
  // the values for the GCC constraint
  val values = Array(1, 2, 3)

  // OscaR's filtering to be tested
  val testedFiltering: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = gccACFiltering(variables)
  }

  // setting the test arguments
  testArguments = Generators.gcc(values)
  testArguments.setSeed(100)

  // the trusted algorithm performing arc consistency
  val trustedFiltering: Filter = new ArcFiltering(Checkers.gccVar(values))

  // checking that the filtered domains returned by both algorithms are the same,
  // i.e that OscaR's filtering reaches arc consistency.
  CPChecker.check(trustedFiltering, testedFiltering)
  // NOTE that here, it does not pass all the tests. We think this is due to the fact
  // that the GCC constraint does not reach the arc consistency for all variables (count variables).


  /*
   * OscaR's filtering algorithm for the GCC constraint
   */
  private def gccACFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val assignment = vars.dropRight(values.length).map(x => CPIntVar(x))
    val cards = vars.drop(vars.length - values.length).map(x => CPIntVar(x))
    val ad = new GCCVarAC(assignment, values(0), cards)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    assignment.map(x => x.toArray.toSet) ++ cards.map(x => x.toArray.toSet)
  }


}