package oscar

import checker._
import CPChecker._
import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.circuit


object CircuitTest {

  def main(args: Array[String]): Unit = {
    val myFilter: Filter = new Filter {
      def filter(variables: Array[Set[Int]]): Array[Set[Int]] = circuitFiltering(variables)
    }
    testArguments.setRangeForAll((0, 4))
    testArguments.setDensityForAll(0.8)
    CPChecker.stronger(new ACFiltering(circuitChecker _), myFilter)
  }

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
      if (sol(index) < 0 || sol(index) >= sol.length)
        return false
      if (visited(sol(index)))
        return false
      visited(sol(index)) = true
      if (acc == sol.length) {
        if (sol(index) == 0)
          return true
        else
          return false
      }
      internal(sol(index), acc + 1)
    }

    internal(0, 1)
  }

}