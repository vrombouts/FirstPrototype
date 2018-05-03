package oscar

import checker.constraints.Constraint
import checker.{NoSolutionException, _}
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffAC
import oscar.cp.core.CPPropagStrength

object AllDifferentACTest {

  def main(args: Array[String]): Unit = {
    val c = new Constraint
    c.gen.setRangeForAll(-5, 5)
    c.checkAC(filteringAllDifferentAC, allDifferent)
    val bugFree : Filter = new ACFiltering(allDifferent)
    val AllDiff : Filter = new Filter {
      override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
        filteringAllDifferentAC(variables)
      }
    }
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setRangeForAll(-5,5)
    CPChecker.check(bugFree, AllDiff)
  }

  def allDifferent(x: Array[Int]): Boolean = x.toSet.size == x.length

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
