# Generic Checker For CP Solver's Constraints : User guide

### Goal of this tool

The goal of the tool is to help developers to test new filtering algorithms for any constraint. The tool can be used to test that the filtering of any constraint is correctly performed over multiple randomly generated domains variables. It can test that the filtering do not remove any solution but also that the domains reduction satisfies some consistencies such as the arc consistency, bound consistency or range consistency. The tool also allows the comparison of two filtering algorithm to see which one is stronger.

### How to check your constraint filtering

The check of a filtering algorithm is based on a comparison with another filtering algorithm. This one should be supposed as bug-free. We call it the trusted filtering algorithm. In order to check your constraint filtering, the tool will generate random domains and apply the filtering functions over them. Then, it will check that your filtering algorithm filters the domains according to the trusted filtering algorithm. The comparison of the trusted and tested filtering algorithms is done by the function `check` or `stronger` of the `CPChecker` object.

The `check` function striclty compare the two filterings. The tested filtering algorithm should not remove more or less values than the trusted filtering algorithm.
The `stronger` function compare the two filterings while supposing that the trusted filtering algorithm removes more values. Therefore, the tested algorithm should remove less or the same number of values as the trusted filtering algorithm. The first argument of those functions should always be the trusted filtering algorithm. Let's show the signature of the `check` function. The `stronger` function's signature will not be shown since it is the same as the `check` function.
```scala
def check(trustedFiltering: Filter, testedFiltering: Filter)
         (implicit testArguments: TestArgs, stats: Statistics): Unit = {
```
As you can see, the filtering algorithm are represented as `Filter` object. This class is an abstract class with a single function: `filter` with this signature: 
```scala
def filter(variables: Array[Set[Int]]): Array[Set[Int]]
```
The `filter` function represents the filtering algorithm. It takes an array of domains (represented as set of integers) and must return the filtered domains. Note that the order of the domains in the array must be respected when returning the filtered domains.

It is important to notice that the `check` function takes implicit parameters. The test arguments contains tell the function how many tests it must perform and the form of the domains generated. This parameter object is explained in the section 'How to modify the domains to your needs'. After the parameters, a second implicit object represents the statistics. It creates files indicating the useful informations about the tests realized such as the number of tests where all value are filtered, the number of tests when one filtering algorithm has remove more values than another, etc.

Before, it was said that this tool allows to check if the tested filtering algorithm reaches some consistencies. This can be done by using special classes of this tool implementing a filtering algorithm (extending the `Filter` class) which respects a certain consistency. Those classes takes in argument for they constructor a checker function. This checker function takes an instantiation of the variables as argument and return true if the relation defined by the constraint is respected. For example, the AllDifferent constraint can have this function as checker: 
```scala
  def allDifferent(instantiation: Array[Int]): Boolean = instantiation.toSet.size == instantiation.length
```
With those checkers it is easy to create the filtering algorithm respecting a particular consistency. For now, There are three consistency filtering algorithm which already exists: the arc consistent (`ACFiltering`), the bound consistent (`BCFiltering`) and the range consistent (`RCFiltering`). Using those classes as the trusted filtering algorithm permits to check a specified consistency.

Let's now see an example. In this example, the AllDifferent constraint is tested. For the OscaR solver has its arc consistent filtering of the AllDifferent constraint tested to see if the algorithm is truly arc consistent.

```scala
import checker.{NoSolutionException, _}
import checker.CPChecker._
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AllDiffAC
import oscar.cp.core.CPPropagStrength

object AllDifferentACTest extends App {

  val oscarAllDifferentAC: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
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
  val CPCheckerAllDifferentAC: Filter = new ACFiltering(Checkers.allDifferent())
  testArguments.setRangeForAll(-5, 5)
  stats.setFolderName("allDifferentAC")
  check(CPCheckerAllDifferentAC, oscarAllDifferentAC)
}
```
In this example, the first step was to create the `Filter` object representing the filtering algorithm of the OscaR solver. Therefore, its `filter` function has been implemented. In it, the variables received in argument are transformed into OscaR variables. Then the AllDifferent constraint is created over those variables. After that, the OscaR variables are filtered. Once they are filtered, they are returned in the asked format.

Once the tested filtering algorithm has been created, the trusted one can be created in one line. Since we test the arc consistency, we can directly use the `ACFiltering` class. For the checker function it needs, it has been retrieved from the `Checkers` object of this tool. This object already provides simple checker for well-known constraint.

Once the filtering algorithms have been created, the implicit arguments are set to suit the need of the constraint. The range of the values of the domains is set to -5 to 5. Then the folder which will contain the statistics is set.

Once all those variables are created, the `check` function can be called. This function will then do all the work. Therefore, the example is finished after its call. And that's it!!



### How to incremetally check your constraint's filtering 

This part is intended to allow you to test the state restoration of variables using a trailing mechanism during the search. To do this, we created 3 functions similar to the previous one but that will in addition test the behaviour of your filtering when doing push/pop operations. For this, we created an object `branchOp` corresponding to a branch operation that is either a push, a pop or a domain restriction. These 3 operations simulates the possible operations that are performed during the search. 

For this part, there are again 3 functions that you can call in order to test your filtering. The first one is the `checkAC` that checks that your filtering removes no solution, that the returned domains satisfy the arc consistency and that when doing branch operations, the state of domains variables is correctly restored. For this, in addition to the generation of random domains, we generate random branch operations (by default 25 but you can modify this number if you wish). Here is the signature of the `checkAC` function dealing with branch operations too : 
```scala  
checkAC(init: Array[Set[Int]] => Array[Set[Int]], filtering: BranchOp => Array[Set[Int]], checker: Array[Int] => Boolean): Unit
```

Similar functions for checking bound consistency and only checking no solution is removed exist as well. Here are their signatures : 
```scala
checkBC(init: Array[Set[Int]] => Array[Set[Int]], filtering: BranchOp => Array[Set[Int]], checker: Array[Int] => Boolean): Unit
check(init: Array[Set[Int]] => Array[Set[Int]], filtering: BranchOp => Array[Set[Int]], checker: Array[Int] => Boolean): Unit
```

When calling one of these functions, you will of course have to define the 3 arguments `init`, `filtering` and `checker`. The `checker` is exacly the same as in the previous part. Here is an example of `checker` that you could define : 
```scala
def checker(sol: Array[Int]): Boolean = {
  if (sol.length == currentVars.length) {
    if (sol.sum == 15) return true
    else return false
  }
  true
}
```
This checker represents the constraint that the sum of the variables must be equal to 15. 

The `init` argument must performs a simple filtering of domains as seen in the previous part too, but must also initialize the solver if needed. By initialization of the solver, we mean that you will have to declare it and to add the constraint to its constraint store (this is only needed if you are testing a constraint filtering belonging to a solver). One example of the `init` function for testing the implementation of the OscaR sum constraint could be : 

```scala
private def init(vars: Array[Set[Int]]): Array[Set[Int]] = {
  solver = CPSolver()
  currentVars = vars.map(x => CPIntVar(x))
  val ad = sum(currentVars).eq(15)
  try {
    solver.post(ad)
  } catch {
    case _: Inconsistency => throw new NoSolutionException
    case _: oscar.cp.core.NoSolutionException => throw new NoSolutionException
  }
  currentVars.map(x => x.toArray.toSet)
}
```
In this function, we first initialize the global variables `solver` and `currentVars`. Then, we add the constraint to the solver's constraint store by posting it. The posting will also do the propagation. We will catch any encountered error and throw a noSolutionException if any. Then, we will return the filtered domains as `Array[Set[Int]]`. 

The `filtering` argument of the `checkAC` function represents again a function that you will have to create. This function takes in argument a branch operation (push, pop or domain restriction) and returns the filtered domains after the operation has been performed and the domains have been filtered. Here is how this function could look like for the given example of the OscaR solver's sum constraint:
```scala
private def filtering(branch: BranchOp): Array[Set[Int]] = {
  branch match {
    case _: Push =>
      solver.propagate()
      solver.pushState()
      currentVars.map(x => x.toArray.toSet)
    case _: Pop =>
      solver.pop()
      currentVars.map(x => x.toArray.toSet)
    case r: RestrictDomain =>
      try {
        val constant = r.constant
        val variable = currentVars(r.index)
        var c: oscar.cp.Constraint = null
        r.op match {
          case "=" => c = new EqCons(variable, constant) // x(i)=constant
          case "<" => c = new Le(variable, constant) // x(i)<constant
          case ">" => c = new Gr(variable, constant) // x(i)>constant
          case "!=" => c = new DiffVal(variable, constant) // x(i)!=constant
          case "<=" => c = new LeEq(variable, constant) // x(i)<=constant
          case ">=" => c = new GrEq(variable, constant) // x(i)>=constant
        }
        solver.post(c)
      } catch {
        case _: oscar.cp.core.NoSolutionException =>
          throw new NoSolutionException
        case _: Inconsistency =>
          throw new NoSolutionException
      }
      currentVars.map(x => x.toArray.toSet)
    case _ => currentVars.map(x => x.toArray.toSet)
  }
}
```
This function can be divided in 4 parts following the type of the `branch` variable. If it is a push operation, we propagate the constraint and push the state. Then, the `currentVars` are returned. If it is a pop operation, we make a pop operation on the `solver` and  we return again the `currentVars`. Since the constraint that has been posted in the `init` involves the `currentVars`, they will be automatically changed when doing a `solver.pop()`. Then, in the case of a domain restriction, a new constaint modelling the domain restriction is added to the solver's constraint store. The domain restriction is necessarily an operation of the type (<,>,<=,>=,=,!=) of one of the domains variables with respect to a constant. The variable index can be accessed while doing a `r.index` with `r` the domain restriction and the constant can be accessed by `r.constant`. We then posted the constraint and make a try catch statement to throw an exception in the case where no possible solution removes. We return the `currentVars`. In order to be consistent, we consider a fourth case where the domain restriction is in another type. This would never be reached.


In the case where you want to change the number of branch operations, simply do the following :
```scala
val c = new Constraint
c.nbBranchOp = 35
```
and then, call your check function over this constraint : 
```scala
c.checkBC(init, filtering, checker)
```

This covers the way you can test your filtering with state restoration. 

### How to modify the domains generator

To test your constraint implementation, we apply it over a number (by default 100) of generated random variables domains. So, we have a generator that creates by default 100 random cases, each of which includes 5 variables with domain of size 4. Each domain contains by default values within the range between -10 and 10.

But you can modify this generator to use different values for each of these parameters (the number of tests, the number of variables,...).

To make a call to the generator that will be used for the tests, simply do the following : 
```scala
val constraint = new Constraint
constraint.gen
```

From this, you can use the set option of the generator to change the number of tests to 120 tests : 
```scala
constraint.gen.setNbTests(120)
```

Similar options exist to change the number of variables or the domains of the variables. Note that for the domains of the variables, two functions can be used. One of them allow you to set all the domains variables to the same ranges and the other one allow you to change only the domain of one variable : 
```scala
constraint.gen.setRangeForAll(-5,5)
// set the domain range of all variables between -5 and 5
  
constraint.gen.setRange(0,(-2,2))
// set the domain range of the first variable between -2 and 2
```

Another option exists, it is the density. The density of a variable is the number of values that belong to its domain over the difference between the maximum and the minimum of this domain. So, since there are 4 possible values for each domain variable and these values are varying between -10 and 10, we have a default density of 4/20 = 0.2. You can also change this density by doing : 
```scala
constraint.gen.setDensityForAll(0.4)
// set the density of all variables to 0.4
  
constraint.gen.setDensity(0,0.5)
// set the density of the first variable to 0.5
```

Finally, a more advanced option that is very important is the seed. By defining a seed, two different calls to the generator using the same seed will generate the same tests. This can be useful to observe the evolution of a particular error while correcting the implementation of your constraint. Here is the way to set the seed :
```scala
constraint.gen.setSeed(123)
```

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

  val c = new Constraint

  //First we set the seed:
  c.gen.setSeed(123456)
  c.gen.setNbTests(124)

  //Then we set x with a size of 7
  c.gen.setNVar(7)
  //add variable i in generator
  c.gen.addVar(0.5, (0, 6))
  //add variable v in generator
  c.gen.addVar(0.1, (-11, 11))
  c.checkAC(elementAC, elementCheck)
}

```
Here is the same constraint but for Choco which is coded in java.

```java
import checker.JCpChecker;
import checker.NoSolutionException;
import checker.constraints.Constraint;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;

public class ElementACTest {

    public static void main(String[] args) {
        Constraint c = new Constraint();
        c.gen().setNVar(7);
        c.gen().addVar(1.5 / 7.0, 0, 10);
        c.gen().addVar(0.1, -10, 10);
        JCpChecker jc = new JCpChecker(new Constraint());
        jc.checkAC(variables -> {
            if (variables.length < 3)
                throw new NoSolutionException("Element must have at least three variables (x,i,v)");
            Model model = new Model("element constraint");
            IntVar[] x = new IntVar[variables.length - 2];
            for (int i = 0; i < variables.length - 2; i++) {
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                x[i] = model.intVar("" + i, b);
            }
            int[] b = variables[variables.length - 2].stream().mapToInt(Number::intValue).toArray();
            IntVar in = model.intVar(b);
            b = variables[variables.length - 1].stream().mapToInt(Number::intValue).toArray();
            IntVar v = model.intVar(b);
            if (x == null) System.out.println("x null!");
            //org.chocosolver.solver.constraints.Constraint cstr = new org.chocosolver.solver.constraints.Constraint(ConstraintsName.ELEMENT,
                 //   new PropElementV_fast(v, x, in, 0, false));
            //model.post(cstr);
            model.element(v,x,in,0).post();
            Solver solver = model.getSolver();
            try {
                solver.propagate();
            } catch (Exception e) {
                throw new NoSolutionException("");
            }
            IntVar[] finalVars = new IntVar[variables.length];
            for (int k = 0; k < variables.length - 2; k++) {
                finalVars[k] = x[k];
            }
            finalVars[variables.length - 2] = in;
            finalVars[variables.length - 1] = v;
            return transform(finalVars);
        }, solution -> {
            if (solution.length < 3) return false;
            int i = solution[solution.length - 2];
            int v = solution[solution.length - 1];
            if (i < 0 || i >= solution.length - 2) return false;
            else return solution[i] == v;
        });
    }


    public static Set<Integer>[] transform(IntVar[] input) {
        Set<Integer>[] result = new Set[input.length];
        for (int i = 0; i < input.length; i++) {
            result[i] = new HashSet<Integer>();
        }
        for (int i = 0; i < input.length; i++) {
            int elem = input[i].getLB();
            int ub = input[i].getUB();
            while (elem != ub) {
                result[i].add(elem);
                elem = input[i].nextValue(elem);
            }
            result[i].add(ub);
        }
        return result;
    }
}


```
