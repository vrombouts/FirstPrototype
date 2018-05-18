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

This part is intended to allow you to test the state restoration of variables using a trailing mechanism during the search. To do this, the object `Filter` from the previous section becomes a `FilterWithState`. This abstract class possesses two function: `setup` and `branchAndFilter`. Those function will permit in addition to test the behaviour of your filtering when doing push/pop operations. For this, we created an object `branchOp` corresponding to a branch operation that is either a push, a pop or a domain restriction. These 3 operations simulates the possible operations that are performed during the search.

For this part, the functions check and stronger can be used in the same way. The only difference is that they take `FilterWithState` objects instead of `Filter`.
```scala
def check(trustedFiltering: FilterWithState, testedFiltering: FilterWithState)
         (implicit testArguments: TestArgs, stats: Statistics): Unit = {
```
Since those function does not use `Filter` objects, the filtering `ACFiltering`, `BCFiltering` and `RCFiltering` cannot be applied directly. Hopefully, this tool provide a class extending the `FilterWithState` class to solve this problem. The `IncrementalFiltering` class takes in argument a `Filter` object to become a `FilterWithState` representing this `Filter` object. 
Here is the signature of the `IncrementalFiltering` class:
```scala
class IncrementalFiltering(filter: Filter) extends FilterWithState {
```

Let's now see the `FilterWithState` abstract class in more details.
```scala
abstract class FilterWithState {
  def setup(variables: Array[Set[Int]]): Array[Set[Int]]

  def branchAndFilter(branching: BranchOp): Array[Set[Int]]
}
```
The `setup` function is used at the start of a test. During this function, the object should instantiate its solver, the variables it receives in argument and the constraint tested then reach the fix-point. It finally returns the filtered domains. From there, the `setup` function is similar to the `filter` function of the `Filter` class. The only difference is that after the `setup` function, the object should be able to access the current solver created with the constraint and the variables. This will allow to do a pseudo-search with the `branchAndFilter` function.

The `branchAndFilter` function takes as argument a `BranchOp` object. This object represent the branching operation the function should perform. It can be three operations. 

         1. Push: The current state of the solver and the variables should be pushed in the the trail.
         2. Pop: the current state of the solver should be reset to the last pushed state.
         3. RestrictDomain: One of the variables should be restricted as informed by in this object.
         
With those possible actions, the `branchAndFilter` function observes which one it should do and do it. If there was a restriction of a domain, the variables have changed and therefore, a new fix-point should be reached by filtering the variables.

The different branching operation are not given randomly. They perform dives. It means that until a solution has been found or an inconsistency happened, the two operations Push and RestrictDomain will be successively used. Then, once a leaf (when there is an inconsistency or a solution is found) has been reached, a random number of Pop will be performed. This represents a dive. By default, this tool does 10 dives per random input domains given to the setup function.

Let's see a concrete example to have a better idea of how it works.
This time, the constraint tested will be the sum. More precisely, the sum of all the values of an instantiation must be equal to 15. It is still an example using the OscaR solver. Here, the filtering algorithm of OscaR for the sum constraint is bound consistent. Hence the `BCFiltering` class will be used to create the trusted filtering algorithm.
```scala
import checker.incremental._
import checker.{NoSolutionException, _}
import CPChecker._
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints._

object SumBCIncrTest extends App {

  val trusted = new IncrementalFiltering(new BCFiltering(Checkers.sum(15, "=")))
  val tested = new FilterWithState {
    implicit private var solver: CPSolver = new CPSolver
    private var currentVars: Array[CPIntVar] = _
   
    override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = {
      solver = CPSolver()
      currentVars = vars.map(x => CPIntVar(x))
      val ad = sum(currentVars).eq(15)
      try {
        solver.post(ad)
      } catch {
        case _: Inconsistency => throw new NoSolutionException
        case _: oscar.cp.core.NoSolutionException => 
          throw new NoSolutionException
      }
      currentVars.map(x => x.toArray.toSet)
    }
   
    override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = {
      branch match {
      case _: Push => solver.pushState()
      case _: Pop => solver.pop()
      case r: RestrictDomain =>
        try {
          val variable = currentVars(r.index)
          val constant = r.constant
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
      }
      currentVars.map(x => x.toArray.toSet)
    }
  }
  check(trusted, tested)
}
```

You can see in this example that the trusted filtering algorithm can still be created in one line thanks to the `IncrementalFiltering`, `BCFiltering` classes and the `Checkers` object. 

The tested filtering algorithm is more complex. The `setup` function is similar to the `filter` function from the previous example but, as said before, the solver and the variables are global variables.

Since the solver is a global variable, the `branchAndFilter` function can access them. With that, this function first look at the branching operation `branch` to choose what it should do. The push and pop can both be done in one line but the restriction of domains takes a little more. a `RestrictDomain` possesses three pieces of information to know which restrictionmust be applied. The first is the index telling which variable is restricted. The second and third are a constant and the relation linking the constant to the variables. For now, this tool possesses 6 types of operations. Therefore, the operation receied is one of those six kinds. Once the constraint representing the restriction has been created, it is posted to the solver. When the constraint is posted, OscaR automatically reach the fix-point and therefore, the variables are filtered. At the end of the `branchAndFilter` function, you should not forget to return the current domains in the generic format of an array of sets of integers.

This conclude the incremental part this tool. If this example was not enough, do not hesitate to go see the other examples in the folder src/main/examples.

### How to modify the domains generator

To test your constraint implementation, we apply it over a number (by default 100) of generated random variables domains. So, we have a generator that creates by default 100 random cases, each of which includes 5 variables with domain of size 4. Each domain contains by default values within the range between -10 and 10.

But you can modify those arguments to use different values for each of these parameters (the number of tests, the number of variables,...).

To create your own parameters that will be used for the tests, simply do the following : 
```scala
implicit val testArgs = new TestArgs
```
Sinc they are implicit you will not even need to pass it to the `check` or `stronger` function.

From this variable, you can, for example, set the number of tests to 120 tests : 
```scala
testArgs.setNbTests(120)
```

Similar options exist to change the number of variables or the domains of the variables. Note that for the domains of the variables, two functions can be used. One of them allow you to set all the domains variables to the same ranges and the other one allow you to change only the domain of one variable : 
```scala
testArgs.setRangeForAll(-5,5)
// set the domain range of all variables between -5 and 5
  
testArgs.setRange(0,(-2,2))
// set the domain range of the first variable between -2 and 2
```

Another option exists, it is the density. The density of a variable is the number of values that belong to its domain over the difference between the maximum and the minimum of this domain. So, since there are 4 possible values for each domain variable and these values are varying between -10 and 10, we have a default density of 4/20 = 0.2. You can also change this density by doing : 
```scala
testArgs.setDensityForAll(0.4)
// set the density of all variables to 0.4
  
testArgs.setDensity(0,0.5)
// set the density of the first variable to 0.5
```

Finally, a more advanced option that is very important is the seed. By defining a seed, two different calls to the `check` or `stronger` function using the same seed will generate the same tests (even the dives perform are the same if the test is incremental). This can be useful to observe the evolution of a particular error while correcting the implementation of your constraint. Here is the way to set the seed :
```scala
testArgs.setSeed(123)
```

For the incremental testing, the default number of dives is fixed to 10 but it can be set to any number with the test arguments.
```scala
testArgs.nbDive = 25
```
