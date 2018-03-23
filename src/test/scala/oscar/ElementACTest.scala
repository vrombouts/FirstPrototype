package oscar

import checker.constraints.Constraint
import checker.NoSolutionException
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.ElementVarAC
import oscar.cp.core.CPPropagStrength


/*
 * this object test the ElementVarAC constraint of OscaR.
 * for this constraint, we need :
 *  - an array of variables x
 *  - a variable i with its domain being indices of x
 *  - a variable v
 *  Then the constraint is x[i]=v
 */
object ElementACTest extends App {
  var size = 0

  /*
   * This function apply the ElementVarAC constraint of OscaR on the variables
   * passed in argument in this format: vars = [x1,x2,...xn, i,v]
   * It then return those variables filtered
   */
  private def elementAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    size = vars.length
    val variables = vars.dropRight(2).map(x => CPIntVar(x))
    val i = CPIntVar(vars(vars.length - 2))
    val v = CPIntVar(vars(vars.length - 1))
    val ad = new ElementVarAC(variables, i, v)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet) ++ Array(i.toArray.toSet, v.toArray.toSet)
  }

  /*
   * This function return true if the solution passed in argument
   * respect the element constraint with its last two element being
   * the variables i and v (solution = [x1,x2,..xn, i,v]
   */
  private def elementCheck(solution: Array[Int]): Boolean = {
    if (size == solution.length) {
      val i = solution(size - 2)
      val v = solution(size - 1)
      return v == solution(i)
    }
    true
  }

  //First we set the seed:
  Constraint.gen.setSeed(123456)
  Constraint.gen.setNbTests(124)

  //Then we set x with a size of 7
  Constraint.gen.setNVar(7)
  //add variable i in generator
  Constraint.gen.addVar(0.5, (0, 6))
  //add variable v in generator
  Constraint.gen.addVar(0.1, (-11, 11))
  print(Constraint.gen.toString)
  Constraint.checkAC(elementAC, elementCheck)
}