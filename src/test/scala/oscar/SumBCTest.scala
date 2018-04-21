package oscar

import checker.constraints.Sum
import checker.{NoSolutionException, ScCpChecker}
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.core.CPPropagStrength

object SumBCTest extends App {
  private def GT(vars: Array[Set[Int]]): Array[Set[Int]] = {
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

  val s:Sum = new Sum(">",15)
  s.checkBC(GT, null)
}
