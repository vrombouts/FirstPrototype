package unit

import java.util

import checker.NoSolutionException
import checker.constraints.Constraint
import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}

class CheckACTests extends UnitSpec {

  def dummyConstraint(x: Array[Set[Int]]): Array[Set[Int]] = x

  @throws[NoSolutionException]
  def throwExceptionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = throw new NoSolutionException

  def noSolutionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = Array.fill(x.length)(Set[Int]())

  def trueChecker(x: Array[Int]): Boolean = true

  def falseChecker(x: Array[Int]): Boolean = false

  Constraint.gen.reset()
  Constraint.gen.setSeed(100)
  "Calling CheckAC for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    Constraint.checkAC(dummyConstraint, falseChecker)
    assert(Constraint.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    Constraint.checkAC(throwExceptionConstraint, trueChecker)
    assert(Constraint.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    Constraint.checkAC(noSolutionConstraint, trueChecker)
    assert(Constraint.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    Constraint.checkAC(dummyConstraint, trueChecker)
    assert(Constraint.stats.nbFailedTests == 0)
  }

  "Calling CheckAC with all tests correct " should "perform 100 tests" in {
    Constraint.checkAC(dummyConstraint, trueChecker)
    assert(Constraint.stats.getNbExecutedTests == 100)
  }

  "Calling CheckAC after having set a generator " should " consider the good generator " in {
    Constraint.gen.setNbTests(150)
    Constraint.gen.setRangeForAll((-5, 5))
    Constraint.gen.setRange(1, (-2, 2))
    Constraint.gen.setDensity(4, 0.3)
    Constraint.gen.setSeed(125)
    Constraint.checkAC(dummyConstraint, trueChecker)
    assert(Constraint.stats.getGenerator == Constraint.gen)
    assert(Constraint.stats.getNbExecutedTests == 150)
  }

  "Calling CheckAC Incremental on a constraint that returns always true with an init removing some values " should " detect at least a failed test" in {
    Constraint.gen.reset()
    var currentVars: Array[Set[Int]] = Array()
    val stack: util.Stack[Array[Set[Int]]] = new util.Stack[Array[Set[Int]]]()

    def dummy(x: Array[Set[Int]]): Array[Set[Int]] = {
      currentVars = dummyConstraint(x)
      if (x(4).size > 1) currentVars(4) = x(4).tail
      currentVars
    }

    def dummyBranchingFiltering(b: BranchOp): Array[Set[Int]] = {
      b match {
        case _: Push => stack.push(currentVars)
        case _: Pop => {
          currentVars = stack.pop(); currentVars
        }
        case r: RestrictDomain => {
          currentVars = r.applyRestriction
          currentVars
        }
      }
    }

    Constraint.checkAC(dummy, dummyBranchingFiltering, trueChecker)
    assert(Constraint.stats.nbFailedTests > 0)
  }

  "Calling CheckAC Incremental on a constraint that returns always true with an init removing no value " should " detect no failed test" in {
    Constraint.gen.reset()
    var currentVars: Array[Set[Int]] = Array()
    var stack: util.Stack[Array[Set[Int]]] = new util.Stack[Array[Set[Int]]]()

    def dummy(x: Array[Set[Int]]): Array[Set[Int]] = {
      currentVars = dummyConstraint(x)
      currentVars
    }

    def dummyBranchingFiltering(b: BranchOp): Array[Set[Int]] = {
      b match {
        case _: Push => stack.push(currentVars)
        case _: Pop => {
          currentVars = stack.pop(); currentVars
        }
        case r: RestrictDomain => {
          currentVars = r.applyRestriction
          currentVars
        }
      }
    }

    Constraint.checkAC(dummy, dummyBranchingFiltering, trueChecker)
    assert(Constraint.stats.nbFailedTests == 0)
  }
}

