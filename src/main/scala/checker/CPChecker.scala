package checker

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}
import org.scalacheck.Prop.forAll

/**
  * This object represents the core of the tool. It contains the check and stronger
  * functions to compare two filtering algorithms.
  */
object CPChecker {

  // the test parameters and statistics responsible of the output
  implicit var testArguments: TestArgs = new TestArgs
  implicit var stats: Statistics = new Statistics("")

  /**
    * Statically check if the reduced domains returned by the testedFiltering and bugFreeFiltering are the same
    * over some random instances.
    *
    * @param bugFreeFiltering : the bug free filtering serving as reference for the comparison
    * @param testedFiltering  : the filtering that should be tested
    * @param testArguments    : the test parameters
    * @param stats            : the statistics responsible of the output
    * @return true if it passes all tests and false otherwise
    */
  def check(bugFreeFiltering: Filter, testedFiltering: Filter)
           (implicit testArguments: TestArgs, stats: Statistics): Boolean = {
    var result: Boolean = true
    // Scalacheck's forall statement to check the inner property
    forAll(testArguments.gen) { x =>
      if ((x.length < testArguments.getNbVars) || checkEmpty(x)) true
      else if (!checkConstraint(x.toArray, bugFreeFiltering, testedFiltering, comparisonCheck(_, null, stats))) {
        result = false
        false
      }
      else true
    }.check(testArguments.getTestParameters)
    stats.setGenerator(testArguments)
    // prints the output
    stats.print
    result
  }

  /**
    * Statically check if the reduced domains returned by the strongerFiltering are included in the reduced domains
    * by the filtering over some random instances.
    *
    * @param strongerFiltering : the stronger filtering serving as reference for the comparison
    * @param filtering         : the filtering that should be tested
    * @param testArguments     : the test parameters
    * @param stats             : the statistics responsible of the output
    * @return true if it passes all tests and false otherwise
    */
  def stronger(strongerFiltering: Filter, filtering: Filter)
              (implicit testArguments: TestArgs, stats: Statistics): Boolean = {
    var result: Boolean = true
    // ScalaCheck's forall to test the inner property
    forAll(testArguments.gen) { x =>
      if ((x.length < testArguments.getNbVars) || checkEmpty(x)) true
      else if (!checkConstraint(x.toArray, strongerFiltering, filtering, comparisonStronger(_, null, stats))) {
        result = false
        false
      }
      else true
    }.check(testArguments.getTestParameters)
    stats.setGenerator(testArguments)
    // prints the output
    stats.print
    result
  }

  /**
    * Incrementally check if the reduced domains returned by the testedFiltering and bugFreeFiltering are the same
    * over some random instances.
    *
    * @param bugFreeFiltering : the bug free filtering serving as reference for the comparison
    * @param testedFiltering  : the filtering that should be tested
    * @param testArguments    : the test parameters
    * @param stats            : the statistics responsible of the output
    * @return true if it passes all tests and false otherwise
    */
  def check(bugFreeFiltering: FilterWithState, testedFiltering: FilterWithState)
           (implicit testArguments: TestArgs, stats: Statistics): Boolean = {
    var result = true
    // ScalaCheck's forall statement to check the inner property
    forAll(testArguments.gen) { x =>
      if ((x.length < testArguments.getNbVars) || checkEmpty(x)) true
      else if (!checkConstraint(x.toArray, bugFreeFiltering, testedFiltering, comparisonCheck(_, _, stats), testArguments)) {
        result = false
        false
      }
      else true
    }.check(testArguments.getTestParameters)
    // prints the output
    stats.setGenerator(testArguments)
    stats.print(true)
    result
  }

  /**
    * Incrementallly check if the reduced domains returned by the filtering are included into
    * the reduced domains returned by the strongerFiltering over some random instances.
    *
    * @param strongerFiltering : the stronger filtering serving as reference for the comparison
    * @param filtering         : the filtering that should be tested
    * @param testArguments     : the test parameters
    * @param stats             : the statistics responsible of the output
    * @return true if it passes all tests and false otherwise
    */
  def stronger(strongerFiltering: FilterWithState, filtering: FilterWithState)
              (implicit testArguments: TestArgs, stats: Statistics): Boolean = {
    var result = true
    // ScalaCheck's forall statement to check the inner condition
    forAll(testArguments.gen) { x =>
      if ((x.length < testArguments.getNbVars) || checkEmpty(x)) true
      else if (!checkConstraint(x.toArray, strongerFiltering, filtering, comparisonStronger(_, _, stats), testArguments)) {
        result = false
        false
      }
      else true
    }.check(testArguments.getTestParameters)
    stats.setGenerator(testArguments)
    // prints the output
    stats.print(true)
    result
  }

  /**
    *
    * @param size      : the number of variables
    * @param parameter : argument for the 'filtering' function
    * @param filtering : function returning filtered domains
    * @tparam T : the type of 'parameter'
    * @return : the filtered domains by the 'filtering' function with 'parameter' as argument1
    */
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

  /**
    *
    * returns true if the domains that have been reduced by the bugFree filtering are the same that the domains being reduced by the tested filtering
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


  /**
    *
    * @param returnValues : the values returned after the application of the two filtering algorithms
    * @param b            : the list of branch operations to be considered
    * @param stats        : responsible of the output
    * @return : true if the returned domains by the bugFree filtering are the same as the returned domains bby the tested
    */
  def comparisonCheck(returnValues: Array[Array[Set[Int]]], b: List[BranchOp] = null, stats: Statistics): Boolean = {
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

  /**
    *
    * @param returnValues : the values returned after the application of the two filtering algorithms
    * @param b            : the list of branch operations to be considered
    * @param stats        : responsible of the output
    * @return : true if the returned domains by the bugFree filtering are included in the returned domains bby the tested
    */
  def comparisonStronger(returnValues: Array[Array[Set[Int]]], b: List[BranchOp] = null, stats: Statistics): Boolean = {
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val bugFreeReducedDomains: Array[Set[Int]] = returnValues(2)
    var result: Boolean = false // incorrect format
    if (correctFormat(reducedDomains, bugFreeReducedDomains)) {
      result = !(bugFreeReducedDomains zip reducedDomains).exists(x => !x._1.subsetOf(x._2))
    }
    stats.updateStats(returnValues, b, result)
    result
  }

  ////STATIC LOGIC /////////

  /**
    *
    * @param variables        : the variables to which we apply the filtering algorithms
    * @param bugFreeFiltering : the trusted algorithm serving as reference for the comparison
    * @param testedFiltering  : the algorithm to be tested
    * @param comparison       : the function to compare the bugFree filtering and the tested filtering
    * @return true if after the application of the filtering algorithms, the comparison returns true, false otherwise
    */
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

  /**
    *
    * @param variables : array of domains
    * @return : true if at least one domain is empty, false otherwise
    */
  private[this] def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

  /**
    *
    * @param variables        : Array of domains to which the filterings will be applied
    * @param bugFreeFiltering : the filtering serving as reference for the testing
    * @param testedFiltering  : the filtering to be tested
    * @param comparison       : the function making the comparison of the returned domains
    *                         by the two filtering functions
    * @param testArguments    : the test parameters
    * @return : true if the comparison function returns true after all branchings, false otherwise
    */
  def checkConstraint(variables: Array[Set[Int]],
                      bugFreeFiltering: FilterWithState,
                      testedFiltering: FilterWithState,
                      comparison: ((Array[Array[Set[Int]]], List[BranchOp]) => Boolean),
                      testArguments: TestArgs)
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
    // Beginning of the algorithm to perform dives
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
    // end of the algorithm to perform dives
    true
  }

  /**
    *
    * @param variables : array of domains
    * @return : true if the variables possess one empty domain or if all variables are fixed (of size 1)
    */
  private[this] def isLeaf(variables: Array[Set[Int]]): Boolean = {
    variables.exists(x => x.isEmpty) || variables.forall(x => x.size == 1)
  }
}
