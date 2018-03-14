package checker.constraints

import Conversions._
import checker.NoSolutionException
import checker.constraints.incremental._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Random

trait Checker {
  protected def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

  protected def printer(initial: Array[Set[Int]], trueReduced: Array[Set[Int]], reduced: Array[Set[Int]], error: Boolean, ourError: Boolean): Unit = {
    if (error && !ourError) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you returned an exception")
    } else if (!error && ourError) {
      println("failed for: " + initial.toList)
      println("you should not have any solutions")
      println("but you had: " + reduced.toList)
    } else if (!error && !ourError) {
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
    var reducedDomains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var ourError: Boolean = false
    try {
      reducedDomains = constraintTested(variables.clone())
    }
    catch {
      //TODO check if it is not better to have a case of NoSolutionException instead
      case _: NoSolutionException => error = true
    }
    // Then we generate the domains that reducedDomains should have
    var trueReducedDomains: Array[Set[Int]] = Array()
    try {
      trueReducedDomains = applyConstraint(variables.clone())
    }
    catch {
      case _: NoSolutionException => ourError = true // doesn't catch java.lang.StackOverflowError
    }
    //Finally, we compare the two. If they are not equals, the constraint is not correct.
    comparison(error, ourError, variables, reducedDomains, trueReducedDomains)
  }

  private def comparison(error: Boolean, ourError: Boolean, variables: Array[Set[Int]], reducedDomains: Array[Set[Int]], ourReducedDomains: Array[Set[Int]]): Boolean = {
    if (error && ourError) return true

    if (error && !ourError) {
      for (i <- ourReducedDomains.indices) {
        if (ourReducedDomains(i).nonEmpty) {
          printer(variables, ourReducedDomains, reducedDomains, error, ourError)
          return false
        }
      }
    }
    else if (!ourError) {
      for (i <- ourReducedDomains.indices) {
        if (!ourReducedDomains(i).equals(reducedDomains(i))) {
          printer(variables, ourReducedDomains, reducedDomains, error, ourError)
          return false
        }
      }
    }
    else {
      //empty domains accepted as having no solutions
      if (reducedDomains.nonEmpty && reducedDomains.forall(x => x.nonEmpty)) {
        printer(variables, ourReducedDomains, reducedDomains, error, ourError)
        return false
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
    domainsStorage.push(currentDomain);
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
    var reducedDomains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var ourError: Boolean = false
    try {
      reducedDomains = init(variables.clone())
    }
    catch {
      //TODO check if it is not better to have a case of NoSolutionException instead
      case _: NoSolutionException => error = true
    }
    // Then we generate the domains that reducedDomains should have
    var ourReducedDomains: Array[Set[Int]] = Array()
    try {
      ourReducedDomains = applyConstraint(variables.clone())
    }
    catch {
      case _: NoSolutionException => ourError = true
    }
    if (!comparison(error, ourError, variables, reducedDomains, ourReducedDomains))
      return false
    if (error && ourError) return true
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
      var reducedDomains: Array[Set[Int]] = Array()
      var error: Boolean = false
      var ourError: Boolean = false
      try {
        reducedDomains = constraintTested(b)
      }
      catch {
        //TODO check if it is not better to have a case of NoSolutionException instead
        case _: NoSolutionException => error = true
      }
      // Then we generate the domains that reducedDomains should have
      var ourReducedDomains: Array[Set[Int]] = Array()
      try {
        ourReducedDomains = applyConstraint(b)
      }
      catch {
        case _: NoSolutionException => ourError = true
      }
      // compare our domains filtered with the ones of the user
      // and if no difference, update the domains for the next branching operation
      if (comparison(error, ourError, variables, reducedDomains, ourReducedDomains, branches))
        vars = ourReducedDomains.clone()
      else
        return false
    }
    println(branches)
    true
  }


  private def comparison(error: Boolean, ourError: Boolean, variables: Array[Set[Int]], reducedDomains: Array[Set[Int]], ourReducedDomains: Array[Set[Int]], b: List[BranchOp]): Boolean = {
    //Finally, we compare the two. If they are not equals, the constraint is not correct.
    if (error && ourError) return true

    if (error && !ourError) {
      for (i <- ourReducedDomains.indices) {
        if (ourReducedDomains(i).nonEmpty) {
          printer(variables, ourReducedDomains, reducedDomains, error, ourError)
          println("with all those branches in reverse order: " + b)
          return false
        }
      }
    }
    else if (!ourError) {
      for (i <- ourReducedDomains.indices) {
        if (!ourReducedDomains(i).equals(reducedDomains(i))) {
          printer(variables, ourReducedDomains, reducedDomains, error, ourError)
          println("with all those branches in reverse order: " + b)
          return false
        }
      }
    }
    else {
      //empty domains accepted as having no solutions
      if (reducedDomains.nonEmpty && reducedDomains.forall(x => x.nonEmpty)) {
        printer(variables, ourReducedDomains, reducedDomains, error, ourError)
        println("with all those branches in reverse order: " + b)
        return false
      }
    }
    true
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
        lastPush = true;
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