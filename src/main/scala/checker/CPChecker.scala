package checker

import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}
import org.scalacheck.Prop.forAll


object CPChecker {

  private[this] var generator: VariablesGenerator = new VariablesGenerator

  def check(bugFreeFiltering: Filter, testedFiltering: Filter)
           (implicit generator: VariablesGenerator): Unit = {
    this.generator = generator
    val stats = new StrictStatistics(20,"AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, bugFreeFiltering, testedFiltering,stats)
    }.check(generator.getTestParameters)
    stats.setGenerator(generator)
    stats.print
  }

  def stronger(strongerFiltering: Filter, filtering: Filter)
              (implicit generator: VariablesGenerator): Unit = {
    this.generator = generator
    val stats = new UnstrictStats(20,"AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
      checkConstraint(x.toArray, strongerFiltering, filtering,stats)
    }
    stats.setGenerator(generator)
    stats.print
  }

  def check(bugFreeFiltering: FilterWithState, testedFiltering: FilterWithState)
           (implicit generator: VariablesGenerator): Unit = {
    val stats = new StrictStatistics(20,"AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, bugFreeFiltering, testedFiltering,stats)
    }.check(generator.getTestParameters)
    stats.setGenerator(generator)
    stats.print
  }

  def stronger(strongerFiltering: FilterWithState, filtering: FilterWithState)
           (implicit generator: VariablesGenerator): Unit = {
    val stats = new UnstrictStats(20,"AC")
    forAll(generator.gen) { x =>
      x.isEmpty || (x.length < generator.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, strongerFiltering, filtering,stats)
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
                     stats:Statistics)
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), testedFiltering.filter),
      apply(variables.length, variables.clone(), bugFreeFiltering.filter))
    //Then, we compare the two. If they are not equals, the constraint is not correct.
    stats.comparison(returnValues)
  }


  ////////INCREMENTAL LOGIC /////////////


  var nbBranchOp: Int = 25

  private[this] def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

  def checkConstraint(variables: Array[Set[Int]],
                      bugFreeFiltering: FilterWithState,
                      testedFiltering: FilterWithState,
                     stats:Statistics)
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
    for (_ <- 0 until nbBranchOp) {
      val b: BranchOp = getBranch(nPush, vars)
      branches ::= b
      b match {
        case _: Push => nPush += 1
        case _: Pop => nPush -= 1
        case _: RestrictDomain =>
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


  private[this] def getBranch(nPush: Int, vars: Array[Set[Int]]): BranchOp = {
    // creation of the table of operations with the operations that are allowed
    // a Pop is not allowed if no push and a restrictDomain is not allowed if all variables are fixed to a value
    var operations: List[BranchOp] = List()
    if (vars.exists(_.isEmpty) && nPush > 0) return new Pop(vars)
    if (!vars.forall(_.size == 1)) {
      if (nPush == 0) {
        lastPush = true
        return new Push(vars)
      }
      if (!lastPush) operations ::= new Push(vars)
      if (nPush > 0 && !lastPush) operations ::= new Pop(vars)
      // give more weight to the RestrictDomain operation
      // since it allows multiple operations (<,>,=,!=,<=,>=)
      for (_ <- 0 until 4)
        operations ::= new RestrictDomain(vars, generator.random)
    } else if (nPush > 0) return new Pop(vars) //nothing to restrict anymore => pop
    //random result
    else return new BranchOp(vars) //should be taken if all Fixed before any branching
    val indexOp = generator.random.nextInt(operations.size)
    if (operations(indexOp).isInstanceOf[Push]) lastPush = true
    else lastPush = false
    operations(indexOp)
  }
}
