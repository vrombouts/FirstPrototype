package checker.constraints

import Conversions._
import checker.{NoSolutionException, VariablesGenerator}
import checker.constraints.incremental._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Random

trait Checker {
  val gen: VariablesGenerator = new VariablesGenerator()

  protected def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

  protected def printer(returnValues:Array[Array[Set[Int]]]): Unit = {
    val initial:Array[Set[Int]] = returnValues(0)
    val reduced:Array[Set[Int]] = returnValues(1)
    val trueReduced:Array[Set[Int]] = returnValues(2)
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
    var returnValues:Array[Array[Set[Int]]] = Array()
    returnValues=returnValues :+ variables
    var reducedDomains: Array[Set[Int]] = Array()
    try {
      reducedDomains = constraintTested(variables.clone())
      returnValues = returnValues:+ reducedDomains
    }
    catch {
      //TODO check if it is not better to have a case of NoSolutionException instead
      case _: NoSolutionException => returnValues = returnValues :+ Array.fill(variables.length)(Set[Int]()) // doesn't catch java.lang.StackOverflowError
    }
    // Then we generate the domains that reducedDomains should have
    var trueReducedDomains: Array[Set[Int]] = Array()
    try {
      trueReducedDomains = applyConstraint(variables.clone())
      returnValues = returnValues:+trueReducedDomains
    }
    catch {
      case _: NoSolutionException => returnValues = returnValues :+ Array.fill(variables.length)(Set[Int]()) // doesn't catch java.lang.StackOverflowError
    }
    //Finally, we compare the two. If they are not equals, the constraint is not correct.
    comparison(returnValues)
  }

  /*
   * returns true if the domains that have been reduced by our function are the same that the domains being reduced by the user function
   */
  private def comparison(returnValues: Array[Array[Set[Int]]]): Boolean = {
    val ourReducedDomains:Array[Set[Int]] = returnValues(2)
    val reducedDomains:Array[Set[Int]] = returnValues(1)
    if(reducedDomains==null) return false
    if(ourReducedDomains.isEmpty && reducedDomains.isEmpty) return true
    else if(ourReducedDomains.isEmpty && reducedDomains.nonEmpty){
      printer(returnValues)
      return false
    }
    else if(ourReducedDomains.length != reducedDomains.length){
      println("Incorrect output format : you don't return the correct number of domains variables")
      return false
    }
    else {
      for (i <- ourReducedDomains.indices) {
        if (!ourReducedDomains(i).equals(reducedDomains(i))) {
          printer(returnValues)
          return false
        }
      }
    }
    true
  }


  // ------------------------ INCREMENTAL PART ------------------------ //


  private[this] val domainsStorage: mutable.Stack[Array[Set[Int]]] = mutable.Stack()

  def applyConstraint(b: BranchOp): Array[Set[Int]] = {
    var restrictDomain: Array[Set[Int]] = Array()
    b match {
      case _: Push => push(b.domains)
      case _: Pop => pop(b.domains)
      case restriction: RestrictDomain =>
        restrictDomain = restriction.applyRestriction()
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
    var returnValues:Array[Array[Set[Int]]]=Array()
    returnValues = returnValues :+ variables
    var reducedDomains: Array[Set[Int]] = Array()
    try {
      reducedDomains = init(variables.clone())
      returnValues = returnValues :+ reducedDomains
    }
    catch {
      //TODO check if it is not better to have a case of NoSolutionException instead
      case _: NoSolutionException => returnValues=returnValues :+ Array.fill(variables.length)(Set[Int]())
    }
    // Then we generate the domains that reducedDomains should have
    var ourReducedDomains: Array[Set[Int]] = Array()
    try {
      ourReducedDomains = applyConstraint(variables.clone())
      returnValues = returnValues :+ ourReducedDomains
    }
    catch {
      case _: NoSolutionException => returnValues = returnValues :+ Array.fill(variables.length)(Set[Int]())
    }
    if (!comparison(returnValues))
      return false
    if (returnValues(1).isEmpty && returnValues(2).isEmpty) return true
    var vars: Array[Set[Int]] = ourReducedDomains.clone()
    var nPush: Int = 0
    var branches: List[BranchOp] = List()
    for (_ <- 0 until 25) {
      val b: BranchOp = getBranch(nPush, vars)
      branches ::= b
      b match {
        case _: Push => nPush += 1
        case _: Pop => nPush -= 1
        case _: RestrictDomain =>
        case _: BranchOp => //no more BranchOp possible (should happen only if all variables are fixed before any Branch)
          println(branches)
          return true
      }
      // apply our constraint and the constraint of the user for the branchOp b and the domains vars
      var ourReducedDomains:Array[Set[Int]]=Array()
      var reducedDomains:Array[Set[Int]]=Array()
      try {
        reducedDomains = constraintTested(b)
        returnValues(1)=reducedDomains
      }
      catch {
        //TODO check if it is not better to have a case of NoSolutionException instead
        case _: NoSolutionException => returnValues(1)=Array.fill(variables.length)(Set[Int]())
      }
      // Then we generate the domains that reducedDomains should have
      try {
        ourReducedDomains = applyConstraint(b)
        returnValues(2)=ourReducedDomains
      }
      catch {
        case _: NoSolutionException => returnValues(2)=Array.fill(variables.length)(Set[Int]())
      }
      // compare our domains filtered with the ones of the user
      // and if no difference, update the domains for the next branching operation
      if (comparison(returnValues))
        vars = ourReducedDomains.clone()
      else
        return false
    }
    println(branches)
    true
  }


  private def comparison(returnValues:Array[Array[Set[Int]]], b: List[BranchOp]): Boolean = {
    val result:Boolean = comparison(returnValues)
    if(!result)
      println("with all those branches in reverse order: " + b)
    result
  }


  // part to get BranchOp's in an interesting random way (for exemple, avoid doing (push->pop->push->pop->...)
  private[this] val random = new Random
  private[this] var lastPush = false

  private def allFixed(variables: Array[Set[Int]]): Boolean = {
    !variables.exists(x => x.size != 1)
  }

  private def getBranch(nPush: Int, vars: Array[Set[Int]]): BranchOp = {
    // creation of the table of operations with the operations that are allowed
    // a Pop is not allowed if no push and a restrictDomain is not allowed if all variables are fixed to a value
    var operations: List[BranchOp] = List()
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
        operations ::= new RestrictDomain(vars)
    } else if (nPush > 0) return new Pop(vars) //nothing to restrict anymore => pop
    //random result
    else return new BranchOp(vars) //should be taken if all Fixed before any branching
    val indexOp = random.nextInt(operations.size)
    if (operations(indexOp).isInstanceOf[Push]) lastPush = true
    else lastPush = false
    operations(indexOp)
  }

}