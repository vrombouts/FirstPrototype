package checker.constraints

import checker._
import checker.constraints.incremental._
import org.scalacheck.Prop.forAll

import scala.collection.mutable
import scala.language.implicitConversions

trait Checker {
  val gen: VariablesGenerator = new VariablesGenerator()
  var stats: Statistics

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



  def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]]

  def checkConstraint(variables: Array[Set[Int]],
                      constraintTested: Array[Set[Int]] => Array[Set[Int]])
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), constraintTested),
      apply(variables.length, variables.clone(), applyConstraint(_: Array[Set[Int]])))
    //Then, we compare the two. If they are not equals, the constraint is not correct.
    stats.comparison(returnValues)
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
    if (!stats.comparison(returnValues))
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
        case _: Pop => nPush -= 1; stats.incNbBacktracks()
        case _: RestrictDomain => stats.incNbNodes()
        case _: BranchOp => //no more BranchOp possible (should happen only if all variables are fixed before any Branch)
          println(branches)
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
    println(branches)
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