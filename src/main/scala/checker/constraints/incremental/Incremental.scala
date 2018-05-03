package checker.constraints.incremental

import checker.Base

import scala.collection.mutable

trait Incremental extends Base {

  var nbBranchOp: Int = 25

  private[this] val domainsStorage: mutable.Stack[Array[Set[Int]]] = mutable.Stack()

  private[this] def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

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

  def push(currentDomains: Array[Set[Int]]): Array[Set[Int]] = {
    domainsStorage.push(currentDomains)
    currentDomains
  }

  def pop(currentDomains: Array[Set[Int]]): Array[Set[Int]] = {
    if (domainsStorage.nonEmpty)
      domainsStorage.pop()
    else
      currentDomains
  }

  def checkConstraint(variables: Array[Set[Int]],
                      init: Array[Set[Int]] => Array[Set[Int]],
                      constraintTested: BranchOp => Array[Set[Int]])
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), init),
      apply(variables.length, variables.clone(), applyConstraint(_: Array[Set[Int]])))
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
      returnValues(1) = apply(variables.length, b, constraintTested)
      returnValues(2) = apply(variables.length, b, applyConstraint(_: BranchOp))
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
        operations ::= new RestrictDomain(vars, gen.random)
    } else if (nPush > 0) return new Pop(vars) //nothing to restrict anymore => pop
    //random result
    else return new BranchOp(vars) //should be taken if all Fixed before any branching
    val indexOp = gen.random.nextInt(operations.size)
    if (operations(indexOp).isInstanceOf[Push]) lastPush = true
    else lastPush = false
    operations(indexOp)
  }
}
