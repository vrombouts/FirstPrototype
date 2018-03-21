# Generic checker for CPSolver's constraints
In constraint programming, the filtering of constraints are of the upmost importance. 
Therefore, filtering algorithms for each of those constraints are constantly researched.
The tool in this repository can be used to test those filtering algorithms for global constraints.
This tool is coded in Scala and can be used to tests filtering algorithms written in Java and Scala.
## How it works

## code example
To test the filtering of a constraint, you always need to give 2 informations.
* A checker telling if a solution is accepted or not by the constraint tested.
* Your own filtering algorithm of the constraint.
There are also other option that can be used such as 
* modifying the generator of variables.
* setting a seed for the tests.
* getting statistics of the results of this tool.

This tool has already be used on two different CPSolver's: Choco and OscaR.
Here is an example for the element constraint in OscaR where its arc consistency is tested.
```
package oscar

import checker.constraints.Constraint
import checker.{NoSolutionException, ScCpChecker}
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

  //set x with a size of 7
  Constraint.gen.setNVar(7)
  //add variable i in generator
  Constraint.gen.addVar(0.5, (0, 6))
  //add variable v in generator
  Constraint.gen.addVar(0.1, (-11, 11))
  Constraint.checkAC(elementAC, elementCheck)
}

```
Here is the same constraint but for Choco which is coded in java.

```
import checker.NoSolutionException
import checker.constraints.Constraint
import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.ConstraintsName
import org.chocosolver.solver.constraints.nary.element.PropElementV_fast
import org.chocosolver.solver.variables.IntVar

object ChocoElementTest {
  var size = 0

  def main(args: Array[String]) {

    //set the x
    Constraint.gen.setNVar(7)
    Constraint.gen.addVar(1.5 / 7.0, (0, 10))
    Constraint.gen.addVar(0.1, (-10, 10))
    Constraint.checkAC(
      //We give the filtering algorithm
      variables => {
        size = variables.length
        if (variables.length < 3) throw NoSolutionException()
        val model: Model = new Model("sum problem")
        val x: Array[IntVar] = new Array[IntVar](variables.length)
        for (i <- variables.indices) {
          val b: Array[Int] = variables(i).toArray
          x(i) = model.intVar("x" + i, b)
        }
        val i: IntVar = x(x.length - 2)
        val v: IntVar = x(x.length - 1)
        val y: Array[IntVar] = x.dropRight(2)
        val cstr = new org.chocosolver.solver.constraints.Constraint(ConstraintsName.ELEMENT,
          new PropElementV_fast(v, y, i, 0, false))
        model.post(cstr)
        //this commented code do a fast element constraint => it does not enforce AC
        //model.element(v,y,i,0).post()
        try {
          model.getSolver.propagate()
        } catch {
          case _: Exception => throw NoSolutionException();
        }
        val result: Array[Set[Int]] = x.map {
          vari =>
            var s: Set[Int] = Set(vari.getUB)
            var elem = vari.getLB
            while (elem != vari.getUB) {
              s = s + elem
              elem = vari.nextValue(elem)
            }
            s
        }
        result
      },
      //Now we create the checker
      solution => {
        if (size < 3) false
        else if (solution.length == size) {
          val i = solution(size - 2)
          val v = solution(size - 1)
          if (i < 0 || i >= size - 2) false
          else solution(i) == v
        } else true
      }
    )
  }
}

```
