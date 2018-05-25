package oscar

import checker.CPChecker._
import checker._
import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.constraints.{ElementCst, ElementVarAC}
import oscar.cp.core.CPPropagStrength

object ElementTest extends App {
  val myFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = elementACFiltering(variables)
  }
  //First we set the seed:
  testArguments.setNbTests(124)

  val x: Array[Int] = Array(1, 5,4,2,3)

  //Then we set x with a size of 7
  testArguments.setNVar(2)
  //add variable i in generator
  testArguments.setRange(0, (0, 5))
  testArguments.setDensity(0, 0.5)
  //add variable v in generator
  testArguments.setRange(1, (1, 6))
  testArguments.setDensity(1, 0.5)
  testArguments.setSeed(123456)
  check(new HybridFiltering(Array(3,1), elementChecker), myFilter)

  /*
   * This function apply the ElementVarAC constraint of OscaR on the variables
   * passed in argument in this format: vars = [x1,x2,...xn, i,v]
   * It then return those variables filtered
   */
  private def elementACFiltering(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val i = CPIntVar(vars(0))
    val v = CPIntVar(vars(1))
    val ad = new ElementCst(x, i, v)
    try {
      ad.setup(CPPropagStrength.Strong)
      //for(i <- 1 to 10)
        //ad.propagate()
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    Array(i.toArray.toSet, v.toArray.toSet)
  }

  /*
   * This function return true if the solution passed in argument
   * respect the element constraint with its last two element being
   * the variables i and v (solution = [x1,x2,..xn, i,v]
   */
  private def elementChecker: Array[Int] => Boolean = {
    solution => {
      val i = solution(0)
      val v = solution(1)
      i<5 && i>=0 && v == x(i)
    }
  }
}
