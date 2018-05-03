package oscar

import checker.{NoSolutionException, _}
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffBC
import oscar.cp.core.CPPropagStrength

object AllDifferentBCTest {

  def main(args: Array[String]): Unit = {
    val bugFree: Filter = new BCFiltering(allDifferentChecker _)
    val AllDiff: Filter = new Filter {
      override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
        allDiffBCFiltering(variables)
      }
    }
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setRangeForAll(-5, 5)
    CPChecker.check(bugFree, AllDiff)
  }

  private def allDiffBCFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
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

  def allDifferentChecker(x: Array[Int]): Boolean = x.toSet.size == x.length

}
