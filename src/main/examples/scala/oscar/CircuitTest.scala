package oscar

import checker._
import CPChecker._
import checker.filterings.ArcFiltering
import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.circuit

/*
 * Testing OscaR's filtering algorithm for the circuit constraint.
 * Here, we will test that this algorithm does not remove any solution by comparing it
 * with a trusted arc consistent algorithm using CPChecker's stronger function.
 */
object CircuitTest extends App {

  val testedFiltering: Filter = new Filter {
    def filter(variables: Array[Set[Int]]): Array[Set[Int]] = circuitFiltering(variables)
  }

  // setting the test parameters
  testArguments.setRangeForAll((0, 4))
  testArguments.setDensityForAll(0.8)

  val trustedFiltering: Filter = new ArcFiltering(circuitChecker _)

  // checking that the trustedFiltering is stronger than the tested one
  CPChecker.stronger(trustedFiltering, testedFiltering)


  /*
   * OscaR's filtering algorithm for the circuit constraint
   */
  private def circuitFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val solver: CPSolver = CPSolver()
    val currentVars: Array[CPIntVar] = vars.map(x => CPIntVar(x))
    val ad = circuit(currentVars)
    try {
      solver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    currentVars.map(x => x.toArray.toSet)
  }

  def circuitChecker(sol: Array[Int]): Boolean = {
    val visited: Array[Boolean] = Array.fill(sol.length)(false)

    def internal(index: Int, acc: Int): Boolean = {
      if (!sol.indices.contains(index) || visited(index))
        return false
      if (acc == sol.length)
        return index == 0
      visited(index) = true
      internal(sol(index), acc + 1)
    }

    internal(sol(0), 1)
  }
}