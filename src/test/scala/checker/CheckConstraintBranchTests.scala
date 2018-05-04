package checker

import java.util

import checker.constraints.incremental._
import org.scalatest.FlatSpec

class CheckConstraintBranchTests extends FlatSpec {

  val dummyInc : FilterWithState = new FilterWithState {
    var currentVars: Array[Set[Int]] = Array()
    val stack: util.Stack[Array[Set[Int]]] = new util.Stack[Array[Set[Int]]]()

    override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = {
      branching match {
        case _: Push => stack.push(currentVars)
        case _: Pop =>
          currentVars = stack.pop()
          currentVars
        case r: RestrictDomain =>
          currentVars = r.applyRestriction
          currentVars
      }
    }

    override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = {
      currentVars = variables
      currentVars
    }
  }
  implicit val generator: VariablesGenerator = new VariablesGenerator
  generator.setSeed(100)
  generator.setNbTests(1)
  //set seed of CPChecker
  val stats: Statistics = new StrictStatistics(20, "AC")
  "checkConstraint with an empty domain or empty" should "return false if init return non-empty domains" in {
    val inc: FilterWithState = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set(1))
    }
    val a:Array[Set[Int]] = Array()
    val b:Array[Set[Int]] = Array(Set())

    assert(!CPChecker.checkConstraint(a, dummyInc, inc, stats))
    assert(!CPChecker.checkConstraint(b, dummyInc, inc, stats))
  }

  "checkConstraint with an empty domain or empty" should "return true if init return empty or empty domains" in {
    val inc: FilterWithState  = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array()
    }
    val inc2: FilterWithState  = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set())
    }
    val a:Array[Set[Int]] = Array()
    val b:Array[Set[Int]] = Array(Set())
    assert(CPChecker.checkConstraint(a, dummyInc, inc, stats))
    assert(CPChecker.checkConstraint(b, dummyInc, inc2, stats))
  }

  "CheckConstraint with init returning an array of different size" should "be false" in {
    val inc: FilterWithState  = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set(5), Set(5))
    }
    val inc2: FilterWithState  = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set(5))
    }
    val inc3: FilterWithState  = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array()
    }
    val a:Array[Set[Int]] = Array()
    assert(!CPChecker.checkConstraint(Array(Set(5)), dummyInc, inc, stats))
    assert(!CPChecker.checkConstraint(Array(Set(5),Set(5)), dummyInc, inc2, stats))
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, stats))
    assert(!CPChecker.checkConstraint(Array(Set(5)), dummyInc, inc3, stats))
  }

  "CheckConstraint with applyConstraint as init and filtering" should "always be true" in {
    val a:Array[Set[Int]] = Array()
    val b:Array[Set[Int]] = Array(Set())
    val c: Array[Set[Int]] = Array(Set(1, 4, 5), Set())
    assert(CPChecker.checkConstraint(a, dummyInc, dummyInc, stats))
    assert(CPChecker.checkConstraint(b, dummyInc, dummyInc, stats))
    assert(CPChecker.checkConstraint(Array(Set(1)), dummyInc, dummyInc, stats))
    assert(CPChecker.checkConstraint(Array(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3)), dummyInc, dummyInc, stats))
    assert(CPChecker.checkConstraint(c, dummyInc, dummyInc, stats))
    assert(CPChecker.checkConstraint(Array(Set(1, 4, 5, 8, 7), Set(1, 4, 5, 8, 7, 9)), dummyInc, dummyInc, stats))
  }

  "checkConstraint with applyConstraint as init and a dummy filtering" should "return false because of filtering" in {
    var i = 0
    val a = Array(Set(1, 2), Set(1, 2))
    val inc = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = {
        if(branching.isInstanceOf[RestrictDomain]) i += 1
        a
      }

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = dummyInc.setup(variables)
    }
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, stats))
    assert(i == 1)
  }

  "checkConstraint with init throwing an exception" should "consider it as a NoSolutionException" in {
    val a = Array(Set(1), Set(2))
    val b: Array[Set[Int]] = Array(Set())
    val inc = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = throw new Exception()
    }
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, stats))
    assert(CPChecker.checkConstraint(b, dummyInc, inc, stats))
  }

  "checkConstraint with filtering throwing an exception" should "consider it as a NoSolutionException" in {
    val a = Array(Set(1, 2), Set(1, 2))
    val inc = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = throw new Exception()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = dummyInc.setup(variables)
    }
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, stats))
  }
}