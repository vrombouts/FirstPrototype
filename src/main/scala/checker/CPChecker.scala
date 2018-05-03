package checker

import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}
import org.scalacheck.Prop.forAll


object CPChecker {

  private[this] var generator: VariablesGenerator = new VariablesGenerator
  var stats: Statistics = new StrictStatistics(20, "AC")

  def check(bugFreeFiltering: Filter, testedFiltering: Filter)
           (implicit generator: VariablesGenerator): Unit = {
    this.generator = generator
    stats = new StrictStatistics(20, "AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, bugFreeFiltering, testedFiltering, stats)
    }.check(generator.getTestParameters)
    stats.setGenerator(generator)
    stats.print
  }

  def stronger(strongerFiltering: Filter, filtering: Filter)
              (implicit generator: VariablesGenerator): Unit = {
    this.generator = generator
    stats = new UnstrictStats(20, "AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, strongerFiltering, filtering, stats)
    }.check(generator.getTestParameters)
    stats.setGenerator(generator)
    stats.print
  }

  def check(bugFreeFiltering: FilterWithState, testedFiltering: FilterWithState)
           (implicit generator: VariablesGenerator): Unit = {
    stats = new StrictStatistics(20, "AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, bugFreeFiltering, testedFiltering, stats)
    }.check(generator.getTestParameters)
    stats.setGenerator(generator)
    stats.print
  }

  def stronger(strongerFiltering: FilterWithState, filtering: FilterWithState)
              (implicit generator: VariablesGenerator): Unit = {
    stats = new UnstrictStats(20, "AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, strongerFiltering, filtering, stats)
    }.check(generator.getTestParameters)
    stats.setGenerator(generator)
    stats.print
  }


  protected[this] def apply[T](size: Int, parameter: T,
                               filtering: T => Array[Set[Int]]): Array[Set[Int]] = {
    try {
      filtering(parameter)
    }
    catch {
      case _: NoSolutionException => Array.fill(size)(Set[Int]()) // doesn't catch java.lang.StackOverflowError
      case e: Exception => println(e.getClass + " " + e)
        Array.fill(size)(Set[Int]())
    }
  }

  ////STATIC LOGIC /////////

  def checkConstraint(variables: Array[Set[Int]],
                      bugFreeFiltering: Filter,
                      testedFiltering: Filter,
                      stats: Statistics)
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), testedFiltering.filter),
      apply(variables.length, variables.clone(), bugFreeFiltering.filter))
    //Then, we compare the two. If they are not equals, the constraint is not correct.
    stats.comparison(returnValues)
  }


  ////////INCREMENTAL LOGIC /////////////


  private[this] def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

  def checkConstraint(variables: Array[Set[Int]],
                      bugFreeFiltering: FilterWithState,
                      testedFiltering: FilterWithState,
                      stats: Statistics)
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), testedFiltering.setup),
      apply(variables.length, variables.clone(), bugFreeFiltering.setup))
    if (!stats.comparison(returnValues))
      return false
    if (checkEmpty(returnValues(1).toList)) return true
    var vars: Array[Set[Int]] = returnValues(2).clone()
    var nPush: Int = 0
    var branches: List[BranchOp] = List()
    var dives = 0
    var lastPop = false
    while (dives < generator.nbDive) {
      val b: BranchOp = getBranch(nPush, vars)
      branches ::= b
      b match {
        case _: Push =>
          lastPop = false
          nPush += 1
        case _: Pop =>
          if (!lastPop) dives += 1
          lastPop = true
          nPush -= 1
        case _: RestrictDomain => lastPop = false
        case _: BranchOp => //no more BranchOp possible (should happen only if all variables are fixed before any Branch)
          return true
      }
      // apply our constraint and the constraint of the user for the branchOp b and the domains vars
      returnValues(1) = apply(variables.length, b, testedFiltering.branchAndFilter)
      returnValues(2) = apply(variables.length, b, bugFreeFiltering.branchAndFilter)
      // compare our domains filtered with the ones of the user
      // and if no difference, update the domains for the next branching operation
      if (stats.comparison(returnValues, branches))
        vars = returnValues(2).clone()
      else
        return false
    }
    true
  }


  // part to get BranchOp's in an interesting random way (for exemple, avoid doing (push->pop->push->pop->...)
  private[this] var lastPush = false
  private[this] var doNPop = 0

  private[this] def getBranch(nPush: Int, vars: Array[Set[Int]]): BranchOp = {
    if (vars.forall(_.size == 1) && nPush == 0)
      new BranchOp(vars) //no dives possible from the start
    else if (nPush == 0) {
      doNPop = 0
      lastPush = true
      new Push(vars)
    } else if (doNPop > 0) {
      lastPush = false
      new Pop(vars)
    }else if (vars.exists(_.isEmpty) || vars.forall(_.size == 1)) {
      doNPop = generator.random.nextInt(nPush)
      lastPush=false
      new Pop(vars)
    } else if (!lastPush) {
      lastPush = true
      new Push(vars)
    } else {
      lastPush = false
      new RestrictDomain(vars, generator.random)
    }
  }
}
