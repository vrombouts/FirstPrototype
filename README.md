# Generic checker for CPSolver's constraints

[![Build Status](https://travis-ci.org/vrombouts/Generic-checker-for-CP-Solver-s-constraints.svg?branch=master)](https://travis-ci.org/vrombouts/Generic-checker-for-CP-Solver-s-constraints)
[![codecov](https://codecov.io/gh/vrombouts/Generic-checker-for-CP-Solver-s-constraints/branch/master/graph/badge.svg)](https://codecov.io/gh/vrombouts/Generic-checker-for-CP-Solver-s-constraints)


In constraint programming, the filtering of constraints are of the upmost importance. 
Therefore, developers are constantly creating filtering algorithms for each of those constraints.
The tool in this repository can be used to test those filtering algorithms for global constraints.
This tool is coded in Scala and can be used to tests filtering algorithms written in Java and Scala.

## Why using this tool?

Thanks to this tool, you can check that the application of your constraint implementation will not remove any solution. Indeed, this is very important since removing solutions constitutes a serious problem. 

You can also test the correct application of some propagation algorithms such as Arc Consistency and Bound Consistency. In this case, the tool will check that the application of your constraint implementation will not remove any solution but also that after applying the constraint, the propagation algorithm has been respected.

Finally, for constraint implemented using a trailing algorithm, this tool allows to check the trailing procedure used through the search. Indeed, it can be used to check the application of your constraint implementation during the search and what happens when doing state restoration.

## How to use this tool?

### Generation of random variables

To test your constraint implementation, we apply it over a number (by default 100) of generated random variables domains. So, we have a generator that creates by default 100 random cases, each of which includes 5 variables with domain of size 4. Each domain contains by default values within the range between -10 and 10.

Note that you can use different values for each of these parameters (the number of tests, the number of variables,...).

To make a call to the generator that will be used for the tests, simply do the following : 
```scala
  Constraint.gen
```

From this, you can use the set option of the generator to change the number of tests to 120 tests : 
```scala
  Constraint.gen.setNbTests(120)
```

Similar options exist to change the number of variables or the domains of the variables. Note that for the domains of the variables, two functions can be used. One of them allow you to set all the domains variables to the same ranges and the other one allow you to change only the domain of one variable : 
```scala
  Constraint.gen.setRangeForAll(-5,5)
  // set the domain range of all variables between -5 and 5
  
  Constraint.gen.setRange(0,(-2,2))
  // set the domain range of the first variable between -2 and 2
```

Another option exists, it is the density. The density of a variable is the number of values that belong to its domain over the difference between the maximum and the minimum of this domain. So, since there are 4 possible values for each domain variable and these values are varying between -10 and 10, we have a default density of 4/20 = 0.2. You can also change this density by doing : 
```scala
  Constraint.gen.setDensityForAll(0.4)
  // set the density of all variables to 0.4
  
  Constraint.gen.setDensity(0,0.5)
  // set the density of the first variable to 0.5
```

Finally, a more advanced option that is very important is the seed. By defining a seed, two different calls to the generator using the same seed will generate the same tests. This can be useful to observe the evolution of a particular error while correcting the implementation of your constraint. Here is the way to set the seed :
```scala
  Constraint.gen.setSeed(123)
```

### How to check your constraint implementation
Now that you have seen how to set the generator, let's see how to test your implementation. The first function that you can use to test your implementation is the `check` function that simply checks that when applying your constraint implementation, you do not remove any solution.

TO DO

Another option is to use the `checkAC` function that will also check that you do not remove any solution, but also, it will check that your domain filtering is arc consistent. Here is the signature of this function : 
```scala
checkAC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean)
```

The first argument of the function is the `filteringTested` which is a function that takes the variable domains in argument and returns the filtered domains. Note that the domains are represented by the type `Array[Set[Int]]`. So, you have to create a function that takes in argument the domains as `Array[Set[Int]]` and will returned the filtered domains after the filtering through your constraint implementation as `Array[Set[Int]]` too. In the case of inconsistency of the domains, this function should either throw a `NoSolutionException` or return an array of empty sets.

The first step to create such a function is to transform the domains of type `Array[Set[Int]]` to the domain type you are using. For example, for testing the allDifferent constraint of the OscaR solver, you will do something like : 
```scala
def filteringAllDifAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
  // transform the vars in argument into CPIntVar which is the domain type used by OscaR
  val variables = vars.map(x => CPIntVar(x))
  ...
}
```
Then, you have to apply your propagation function. Considering the same example as before, you will do : 
```scala
   def filteringAllDifAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
    // Creation of the OscaR solver 
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong) 
    // Creation of the OscaR variables
    val variables = vars.map(x => CPIntVar(x))
    // Creation of the constraint of OscaR
    val ad = new AllDiffAC(variables)
    try {
      //propagation to fix-point
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    ...
  }
```
In this part, you can see that we create a new constraint allDifferentAC (`ad`) which will be added to the `testSolver` in order to be considered by the solver for the filtering. The `try{...} catch{...}` statement is here to throw the `NoSOlutionException` error in the case of failure.

Now, the domains have been filtered. The only thing that remains is to convert it in the return type (`Array[Set[Int]]`) for the return statement. Here is the complete code containing this last transformation too : 

 ```scala
  def filteringAllDifAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad = new AllDiffAC(variables)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    // conversion of the OscaR domain type into Array[Set[Int]]
    variables.map(x => x.toArray.toSet)
  }
 ```
 
Now, let's consider the second variable of the `checkAC` function. It is the `checker` function which represents the constraint to be tested. This function takes in input an instantiation of the variables and returns true if this instantiation satisfies the constraint, false otherwise. An example of a checker function representing the allDifferent constraint could be :
```scala
def allDiffChecker(x:Array[Int]):Boolean = x.toSet.size == x.length
```

This function is quite simple. By making `x.toSet`, you only have elements that are different that remains. So, taking the size of it would be equal to `x.size` if and only if all values given as input were different. So, this function returns true if all values of `x` are different and false otherwise, which is exactly what we needed.

With all these things, you can test the OscaR implementation of the allDifferent constraint with the following code : 
```scala
import checker.constraints.Constraint
import checker.NoSolutionException
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffAC
import oscar.cp.core.CPPropagStrength

object AllDifferentACTest extends App {
  private def filteringAllDifAC(vars: Array[Set[Int]]): Array[Set[Int]] = {
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

  def allDiffChecker(x:Array[Int]):Boolean = x.toSet.size == x.length
  Constraint.gen.setRangeForAll(-5,5) // change the generator parameters

  Constraint.checkAC(allDifAC,allDiff)
}
```
And that's it! 

We did a similar function `checkBC` that has approximately the same signature and do the same apart from the fact that we check that the filtering respects the bound consistency condition and not the arc consistency. Here is the signatire of this method : 
```scala
checkBC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean)
```

TO DO : add explanations on incremental constraints testing
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
```scala
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

```scala
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
