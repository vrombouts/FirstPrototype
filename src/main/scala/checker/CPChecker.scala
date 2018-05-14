package checker

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}
import org.scalacheck.Prop.forAll


object CPChecker {

  implicit var testArguments: TestArgs = new TestArgs
  implicit var stats: Statistics = new Statistics("")

  def check(bugFreeFiltering: Filter, testedFiltering: Filter)
           (implicit testArguments: TestArgs, stats: Statistics): Unit = {
    this.testArguments = testArguments
    this.stats = stats
    forAll(testArguments.gen) { x =>
      (x.length < testArguments.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, bugFreeFiltering, testedFiltering, comparisonCheck(_))
    }.check(testArguments.getTestParameters)
    stats.setGenerator(testArguments)
    stats.print
  }

  def stronger(strongerFiltering: Filter, filtering: Filter)
              (implicit testArguments: TestArgs, stats: Statistics): Unit = {
    this.testArguments = testArguments
    this.stats = stats
    forAll(testArguments.gen) { x =>
      (x.length < testArguments.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, strongerFiltering, filtering, comparisonStronger(_))
    }.check(testArguments.getTestParameters)
    stats.setGenerator(testArguments)
    stats.print
  }

  def check(bugFreeFiltering: FilterWithState, testedFiltering: FilterWithState)
           (implicit testArguments: TestArgs, stats: Statistics): Unit = {
    this.testArguments = testArguments
    this.stats = stats
    forAll(testArguments.gen) { x =>
      (x.length < testArguments.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, bugFreeFiltering, testedFiltering, comparisonCheck(_, _))
    }.check(testArguments.getTestParameters)
    stats.setGenerator(testArguments)
    stats.print(true)
  }

  def stronger(strongerFiltering: FilterWithState, filtering: FilterWithState)
              (implicit testArguments: TestArgs, stats: Statistics): Unit = {
    this.testArguments = testArguments
    this.stats = stats
    forAll(testArguments.gen) { x =>
      (x.length < testArguments.getNbVars) || checkEmpty(x) ||
        checkConstraint(x.toArray, strongerFiltering, filtering, comparisonStronger(_, _))
    }.check(testArguments.getTestParameters)
    stats.setGenerator(testArguments)
    stats.print(true)
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

  /*
  * returns true if the domains that have been reduced by our function are the same that the domains being reduced by the user function
  */
  private[this] def correctFormat(reducedDomains: Array[Set[Int]], bugFreeReducedDomains: Array[Set[Int]]): Boolean = {
    var errorMsg: String = ""
    if (reducedDomains == null)
      errorMsg = "You returned a null array instead of an array of filtered domains"
    else if (bugFreeReducedDomains.length != reducedDomains.length)
      errorMsg = "Incorrect output format : you don't return the correct number of domains variables"
    if (errorMsg.nonEmpty) println(errorMsg)
    errorMsg.isEmpty
  }

  def comparisonCheck(returnValues: Array[Array[Set[Int]]], b: List[BranchOp] = null): Boolean = {
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val bugFreeReducedDomains: Array[Set[Int]] = returnValues(2)
    var result: Boolean = false // incorrect format
    if (correctFormat(reducedDomains, bugFreeReducedDomains)) {
      if (bugFreeReducedDomains.exists(_.isEmpty) && reducedDomains.exists(_.isEmpty)) result = true
      else if (bugFreeReducedDomains.exists(_.isEmpty) && reducedDomains.forall(_.nonEmpty)) result = false
      else result = !(bugFreeReducedDomains zip reducedDomains).exists(x => !x._1.equals(x._2))
      stats.updateStats(returnValues, b, result)
    }
    result
  }

  def comparisonStronger(returnValues: Array[Array[Set[Int]]], b: List[BranchOp] = null): Boolean = {
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val bugFreeReducedDomains: Array[Set[Int]] = returnValues(2)
    var result: Boolean = false // incorrect format
    if (correctFormat(reducedDomains, bugFreeReducedDomains)) {
      if (bugFreeReducedDomains.exists(_.isEmpty)) {
        // check that if no solution can be found, either you still have unfixed variables
        // or if all variables are instantiated, you should find there is no solution
        result = !reducedDomains.forall(_.size == 1)
      }
      else {
        result = !(bugFreeReducedDomains zip reducedDomains).exists(x => !x._1.subsetOf(x._2))
      }
    }
    stats.updateStats(returnValues, b, result)
    result
  }

  ////STATIC LOGIC /////////

  def checkConstraint(variables: Array[Set[Int]],
                      bugFreeFiltering: Filter,
                      testedFiltering: Filter,
                      comparison: Array[Array[Set[Int]]] => Boolean)
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), testedFiltering.filter),
      apply(variables.length, variables.clone(), bugFreeFiltering.filter))
    //Then, we compare the two. If they are not equals, the constraint is not correct.
    comparison(returnValues)
  }


  ////////INCREMENTAL LOGIC /////////////


  private[this] def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

  def checkConstraint(variables: Array[Set[Int]],
                      bugFreeFiltering: FilterWithState,
                      testedFiltering: FilterWithState,
                      comparison: ((Array[Array[Set[Int]]], List[BranchOp]) => Boolean))
  : Boolean = {
    def comp(b: List[BranchOp]): Array[Set[Int]] = {
      val tested = apply(variables.length, b.head, testedFiltering.branchAndFilter)
      val trusted = apply(variables.length, b.head, bugFreeFiltering.branchAndFilter)
      val result = comparison(Array(variables, tested, trusted), b)
      if (!result) return null
      trusted
    }

    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), testedFiltering.setup),
      apply(variables.length, variables.clone(), bugFreeFiltering.setup))
    if (!comparison(returnValues, null))
      return false
    if (checkEmpty(returnValues(1).toList) || isLeaf(returnValues(2))) return true
    var vars: Array[Set[Int]] = returnValues(2).clone()
    var nPush: Int = 0
    var branches: List[BranchOp] = List()
    var dives = 0
    while (dives < testArguments.nbDive) {
      while (!isLeaf(vars)) {
        branches ::= new Push(vars)
        nPush += 1
        vars = comp(branches)
        if (vars == null) return false
        branches ::= new RestrictDomain(vars.clone(), testArguments.random)
        vars = comp(branches)
        if (vars == null) return false
      }
      dives += 1
      val rd: Int = if (nPush <= 1) 1 else 1 + testArguments.random.nextInt(nPush - 1)
      for (i <- 0 until rd) {
        nPush -= 1
        branches ::= new Pop(vars)
        vars = comp(branches)
        if (vars == null) return false
      }
    }
    true
  }

  private[this] def isLeaf(variables: Array[Set[Int]]): Boolean = {
    variables.exists(x => x.isEmpty) || variables.forall(x => x.size == 1)
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
    } else if (vars.exists(_.isEmpty) || vars.forall(_.size == 1)) {
      doNPop = testArguments.random.nextInt(nPush)
      lastPush = false
      new Pop(vars)
    } else if (!lastPush) {
      lastPush = true
      new Push(vars)
    } else {
      lastPush = false
      new RestrictDomain(vars, testArguments.random)
    }
  }
}
