package oscar

import checker.constraints.Table
import checker.NoSolutionException
import oscar.algo.Inconsistency
import oscar.cp.{table, _}
import oscar.cp.core.CPPropagStrength

object TableACTest extends App {
  val myTable: Set[Array[Int]] = Set(
    Array(1,2,3),
    Array(2,2,3),
    Array(1,3,3),
    Array(1,2,4)
  )

  private def tableAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad = table(variables, myTable.toArray)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }

  val t = new Table(myTable)
  t.checkAC(tableAC, null)
}
