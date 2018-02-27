package OscaR

import checker.{NoSolutionException, ScCpChecker}
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.core.CPPropagStrength

object SumBC extends App{
  private def GT(vars:Array[Set[Int]]):Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.dropRight(1).map(x => CPIntVar(x))
    val constant = vars(vars.length-1).last
    val ad = sum(variables) > constant
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet) ++ Array(vars(vars.length-1))
  }
  ScCpChecker.checkSumGT(GT)
}
