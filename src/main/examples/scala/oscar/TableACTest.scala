package oscar

import checker.{NoSolutionException, _}
import oscar.algo.Inconsistency
import oscar.cp.{table, _}
import oscar.cp.core.CPPropagStrength

object TableACTest {
  val myTable: Set[Array[Int]] = Set(
    Array(1, 2, 3),
    Array(2, 2, 3),
    Array(1, 3, 3),
    Array(1, 2, 4)
  )

  def main(args: Array[String]): Unit = {
    val myFilter: Filter = new Filter {
      override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = tableACFiltering(variables)
    }
    implicit val parameters: TestArgs = Generators.table(myTable)
    CPChecker.check(new ACFiltering(Checkers.table(myTable)), myFilter)
  }

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
