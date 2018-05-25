package oscar

import checker.{NoSolutionException, _}
import CPChecker._
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.core.CPPropagStrength

object SumBCTest extends App {

  val myFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = sumGTFiltering(variables)
  }
  CPChecker.check(new BoundZFiltering(sumChecker _), myFilter)


  private def sumGTFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
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

  def sumChecker(sol: Array[Int]): Boolean = {
    if (sol.sum > 15) true
    else false
  }

}
