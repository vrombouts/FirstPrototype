package OscaR

import checker.{NoSolutionException, ScCpChecker}
import oscar.algo.Inconsistency
import oscar.cp.{table, _}
import oscar.cp.core.CPPropagStrength

object TableAC extends App{
  private def tableAC(vars:Array[Set[Int]],tablee: Set[Array[Int]]):Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad =  table(variables, tablee.toArray)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }

  ScCpChecker.checkTableAC(tableAC)
}
