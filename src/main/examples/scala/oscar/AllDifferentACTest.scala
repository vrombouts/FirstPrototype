package oscar

import checker.{NoSolutionException, _}
import checker.CPChecker._
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffAC
import oscar.cp.core.CPPropagStrength

object AllDifferentACTest {

  def main(args: Array[String]): Unit = {
    val CPCheckerAllDifferentAC: Filter = new ACFiltering(Checkers.allDifferent())
    val oscarAllDifferentAC: Filter = new Filter {
      override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
        filteringAllDifferentAC(variables)
      }
    }
    //implicit val parameters: TestArgs = new TestArgs
    generator.setRangeForAll(-5, 5)
    check(CPCheckerAllDifferentAC, oscarAllDifferentAC)
  }

  def filteringAllDifferentAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad = new AllDiffAC(variables)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }
}
