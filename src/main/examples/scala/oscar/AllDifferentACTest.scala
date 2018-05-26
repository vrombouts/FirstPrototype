package oscar

import checker.{NoSolutionException, _}
import checker.CPChecker._
import checker.prunings.ArcPruning
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffAC
import oscar.cp.core.CPPropagStrength

object AllDifferentACTest extends App {

  val CPCheckerAllDifferentAC: Filter = new ArcPruning(Checkers.allDifferent())
  val oscarAllDifferentAC: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
      filteringAllDifferentAC(variables)
    }
  }
  testArguments.setRangeForAll(-5, 5)
  stats.setFolderName("allDifferentAC")
  check(CPCheckerAllDifferentAC, oscarAllDifferentAC)


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
