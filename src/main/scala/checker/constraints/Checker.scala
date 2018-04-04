package checker.constraints

import Conversions._
import checker.{NoSolutionException, Statistics, VariablesGenerator}
import checker.constraints.incremental._
import org.scalacheck.Prop.forAll

import scala.collection.mutable
import scala.language.implicitConversions

trait Checker {
  val gen: VariablesGenerator = new VariablesGenerator()

  protected def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

  protected def forAllCheck(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringTested)
    }.check(gen.getTestParameters)
  }

  protected def forAllCheck(init: Array[Set[Int]] => Array[Set[Int]],
                            filtering: BranchOp => Array[Set[Int]]): Unit = {
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, init, filtering)
    }.check(gen.getTestParameters)
  }

  protected def printer(returnValues: Array[Array[Set[Int]]]): Unit = {
    val initial: Array[Set[Int]] = returnValues(0)
    val reduced: Array[Set[Int]] = returnValues(1)
    val trueReduced: Array[Set[Int]] = returnValues(2)
    if (reduced.isEmpty && trueReduced.nonEmpty) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you claim there is no solution")
    } else if (reduced.nonEmpty && trueReduced.isEmpty) {
      println("failed for: " + initial.toList)
      println("you should not have any solutions")
      println("but you had: " + reduced.toList)
    } else if (reduced.nonEmpty && trueReduced.nonEmpty) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you had: " + reduced.toList)
    }
  }

  def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]]

  def checkConstraint(variables: Array[Set[Int]],
                      constraintTested: Array[Set[Int]] => Array[Set[Int]])
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), constraintTested),
      apply(variables.length, variables.clone(), applyConstraint(_: Array[Set[Int]])))
    //Then, we compare the two. If they are not equals, the constraint is not correct.
    comparison(returnValues)
  }

  /*
   * returns true if the domains that have been reduced by our function are the same that the domains being reduced by the user function
   */
  private[this] def comparison(returnValues: Array[Array[Set[Int]]], b: List[BranchOp] = null): Boolean = {
    val ourReducedDomains: Array[Set[Int]] = returnValues(2)
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val init: Array[Set[Int]] = returnValues(0)
    var result: Boolean = true
    if (reducedDomains == null) {
      println("You returned a null array instead of an array of filtered domains")
      result = false
    }
    else if (ourReducedDomains.length != reducedDomains.length) {
      println("Incorrect output format : you don't return the correct number of domains variables")
      result = false
    }
    else if (ourReducedDomains.exists(_.isEmpty) && reducedDomains.exists(_.isEmpty)) {
      if (b != null)
        Statistics.incNbLeaves()
      result = true
    }
    else if ((ourReducedDomains.exists(_.isEmpty) && reducedDomains.forall(_.nonEmpty)) ||
      (ourReducedDomains.forall(_.nonEmpty) && reducedDomains.exists(_.isEmpty))) {
      printer(returnValues)
      result = false
    }
    else {
      if (b != null && allFixed(reducedDomains))
        Statistics.incNbLeaves()
      if ((ourReducedDomains zip reducedDomains).exists(x => !x._1.equals(x._2))) {
        printer(returnValues)
        result = false
      }
    }
    if (b != null && !result) println("with all those branches in reverse order: " + b)
    if (b == null) {
      Statistics.incNbExecutedTests()
      if (ourReducedDomains.exists(_.isEmpty)) {
        Statistics.incNbNoSolutionTests()
        if (!result) Statistics.incNbFailedNoSolutionTests()
      }
      else if ((ourReducedDomains zip init).forall(x => x._1.equals(x._2))) {
        Statistics.incNbRemoveNoValueTests()
        if (!result) Statistics.incNbFailedRemoveNoValueTests()
      }
      else {
        Statistics.incNbRemovingValueTests()
        if (!result) Statistics.incNbFailedRemovingValueTests()
      }
    }
    result
  }

  private[this] def apply[T](size: Int, parameter: T,
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


  // ------------------------ INCREMENTAL PART ------------------------ //


  private[this] val domainsStorage: mutable.Stack[Array[Set[Int]]] = mutable.Stack()

  def applyConstraint(b: BranchOp): Array[Set[Int]] = {
    var restrictDomain: Array[Set[Int]] = Array()
    b match {
      case _: Push => push(b.domains)
      case _: Pop => pop(b.domains)
      case restriction: RestrictDomain =>
        restrictDomain = restriction.applyRestriction
        applyConstraint(restrictDomain)
      case _ => b.domains
    }
  }

  def push(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    domainsStorage.push(currentDomain)
    currentDomain
  }

  def pop(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    if (domainsStorage.nonEmpty)
      domainsStorage.pop()
    else
      currentDomain
  }


  def checkConstraint(variables: Array[Set[Int]],
                      init: Array[Set[Int]] => Array[Set[Int]],
                      constraintTested: BranchOp => Array[Set[Int]])
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), init),
      apply(variables.length, variables.clone(), applyConstraint(_: Array[Set[Int]])))
    if (!comparison(returnValues))
      return false
    if (checkEmpty(returnValues(1).toList)) return true
    var vars: Array[Set[Int]] = returnValues(2).clone()
    var nPush: Int = 0
    var branches: List[BranchOp] = List()
    for (_ <- 0 until 25) {
      val b: BranchOp = getBranch(nPush, vars)
      branches ::= b
      b match {
        case _: Push => nPush += 1
        case _: Pop => nPush -= 1; Statistics.incNbBacktracks()
        case _: RestrictDomain => Statistics.incNbNodes()
        case _: BranchOp => //no more BranchOp possible (should happen only if all variables are fixed before any Branch)
          println(branches)
          return true
      }
      // apply our constraint and the constraint of the user for the branchOp b and the domains vars
      returnValues(1) = apply(variables.length, b, constraintTested)
      returnValues(2) = apply(variables.length, b, applyConstraint(_: BranchOp))
      // compare our domains filtered with the ones of the user
      // and if no difference, update the domains for the next branching operation
      if (comparison(returnValues, branches))
        vars = returnValues(2).clone()
      else
        return false
    }
    println(branches)
    true
  }


  // part to get BranchOp's in an interesting random way (for exemple, avoid doing (push->pop->push->pop->...)
  private[this] var lastPush = false

  private[this] def allFixed(variables: Array[Set[Int]]): Boolean = {
    variables.forall(_.size == 1)
  }

  private[this] def getBranch(nPush: Int, vars: Array[Set[Int]]): BranchOp = {
    // creation of the table of operations with the operations that are allowed
    // a Pop is not allowed if no push and a restrictDomain is not allowed if all variables are fixed to a value
    var operations: List[BranchOp] = List()
    if (vars.exists(_.isEmpty) && nPush > 0) return new Pop(vars)
    if (!allFixed(vars)) {
      if (nPush == 0) {
        lastPush = true
        return new Push(vars)
      }
      if (!lastPush) operations ::= new Push(vars)
      if (nPush > 0 && !lastPush) operations ::= new Pop(vars)
      // give more weight to the RestrictDomain operation
      // since it allows multiple operations (<,>,=,!=,<=,>=)
      for (_ <- 0 until 4)
        operations ::= new RestrictDomain(vars,gen.random)
    } else if (nPush > 0) return new Pop(vars) //nothing to restrict anymore => pop
    //random result
    else return new BranchOp(vars) //should be taken if all Fixed before any branching
    val indexOp = gen.random.nextInt(operations.size)
    if (operations(indexOp).isInstanceOf[Push]) lastPush = true
    else lastPush = false
    operations(indexOp)
  }

}