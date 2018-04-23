package oscar

import checker.constraints.Constraint
import checker.NoSolutionException
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffBC
import oscar.cp.core.CPPropagStrength

object AllDifferentBCTest extends App {
  private def allDifBC(vars: Array[Set[Int]]): Array[Set[Int]] = {
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

  def allDiff(x:Array[Int]):Boolean = x.toSet.size == x.length

  val c = new Constraint
  c.gen.setRangeForAll(-5,5)
  c.checkBC(allDifBC,allDiff)

}
