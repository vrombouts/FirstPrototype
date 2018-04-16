package unit

import java.util

import checker.NoSolutionException
import checker.constraints.Constraint
import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}

class CheckTests extends UnitSpec {

  def dummyConstraint(x: Array[Set[Int]]): Array[Set[Int]] = x

  @throws[NoSolutionException]
  def throwExceptionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = throw new NoSolutionException

  def noSolutionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = Array.fill(x.length)(Set[Int]())

  def trueChecker(x: Array[Int]): Boolean = true

  def falseChecker(x: Array[Int]): Boolean = false


  Constraint.gen.setSeed(100)
  "Calling Check for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect no error" in {
    Constraint.check(dummyConstraint, falseChecker)
    assert(Constraint.stats.nbFailedTests == 0)
  }

  "Calling Check for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    Constraint.check(throwExceptionConstraint, trueChecker)
    assert(Constraint.stats.nbFailedTests > 0)
  }

  "Calling Check for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "have at least one error" in {
    Constraint.check(noSolutionConstraint, trueChecker)
    assert(Constraint.stats.nbFailedTests > 0)
  }

  "Calling Check for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    Constraint.check(dummyConstraint, trueChecker)
    assert(Constraint.stats.nbFailedTests == 0)
  }

  "Calling Check with all tests correct " should "perform 100 tests" in {
    Constraint.check(dummyConstraint, trueChecker)
    assert(Constraint.stats.getNbExecutedTests == 100)
  }

  "Calling Check after having set a generator " should " consider the good generator " in {
    Constraint.gen.setNbTests(150)
    Constraint.gen.setRangeForAll((-5, 5))
    Constraint.gen.setRange(1, (-2, 2))
    Constraint.gen.setDensity(4, 0.3)
    Constraint.gen.setSeed(125)
    Constraint.check(dummyConstraint, trueChecker)
    assert(Constraint.stats.getGenerator == Constraint.gen)
    assert(Constraint.stats.getNbExecutedTests == 150)
  }

  "Calling Check Incremental on a constraint that returns always true with an init removing some values " should " detect at least an error " in {
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

    Constraint.check(dummy, dummyBranchingFiltering, trueChecker)
    assert(Constraint.stats.nbFailedTests > 0)
  }

  "Calling Check Incremental on a constraint that returns always true with an init removing no value " should " detect no error" in {
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

    Constraint.check(dummy, dummyBranchingFiltering, trueChecker)
    assert(Constraint.stats.nbFailedTests == 0)
  }

}
