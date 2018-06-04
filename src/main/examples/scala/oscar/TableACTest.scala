package oscar

import checker.{NoSolutionException, _}
import CPChecker._
import checker.filterings.ArcFiltering
import oscar.algo.Inconsistency
import oscar.cp.{table, _}
import oscar.cp.core.CPPropagStrength

/*
 * Checking OscaR's arc consistent filtering for the table constraint.
 * This constraint simply imposes that the solutions must belong to a table of instantiations.
 * Here, we will test that the filtering reaches arc consistency.
 */
object TableACTest extends App {
  // the table for the constraint
  val myTable: Set[Array[Int]] = Set(
    Array(1, 2, 3),
    Array(2, 2, 3),
    Array(1, 3, 3),
    Array(1, 2, 4)
  )

  // OscaR's filtering to be tested
  val testedFiltering: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = tableACFiltering(variables)
  }

  // setting the test parameters
  testArguments = Generators.table(myTable)

  // the trusted filtering performing arc consistency
  val trustedFiltering: Filter = new ArcFiltering(Checkers.table(myTable))

  // checking that the filtered domains returned by both filterings are the same.
  // i.e. checking that OscaR's filtering is arc consistent
  CPChecker.check(trustedFiltering, testedFiltering)


  // OscaR's arc consistent filtering for the table constraint to be tested
  private def tableACFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables: Array[CPIntVar] = vars.map(x => CPIntVar(x))
    val ad = table(variables, myTable.toArray)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }

}
