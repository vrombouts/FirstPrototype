package unit

import java.util

import checker.NoSolutionException
import checker.constraints.Constraint
import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}

/*
 * This class contains the tests for the CheckAC, CheckBC and Check functions
 */
class CheckTests extends UnitSpec {

  def dummyConstraint(x: Array[Set[Int]]): Array[Set[Int]] = x

  @throws[NoSolutionException]
  def throwExceptionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = throw new NoSolutionException

  def noSolutionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = Array.fill(x.length)(Set[Int]())

  def trueChecker(x: Array[Int]): Boolean = true

  def falseChecker(x: Array[Int]): Boolean = false


  "Calling CheckAC for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkAC(dummyConstraint, falseChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkAC(throwExceptionConstraint, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkAC(noSolutionConstraint, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkAC(dummyConstraint, trueChecker)
    assert(c.stats.nbFailedTests == 0)
  }

  "Calling CheckAC with all tests correct " should "perform 100 tests" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkAC(dummyConstraint, trueChecker)
    assert(c.stats.getNbExecutedTests == 100)
  }

  "Calling CheckAC after having set a generator " should " consider the good generator " in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.gen.setNbTests(150)
    c.gen.setRangeForAll((-5, 5))
    c.gen.setRange(1, (-2, 2))
    c.gen.setDensity(4, 0.3)
    c.gen.setSeed(125)
    c.checkAC(dummyConstraint, trueChecker)
    assert(c.stats.getGenerator == c.gen)
    assert(c.stats.getNbExecutedTests == 150)
  }

  "Calling CheckAC Incremental on a constraint that returns always true with an init removing some values " should " detect at least a failed test" in {
    val c = new Constraint
    c.gen.setSeed(100)
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
        case _: Pop =>
          currentVars = stack.pop()
          currentVars
        case r: RestrictDomain =>
          currentVars = r.applyRestriction
          currentVars
      }
    }

    c.checkAC(dummy, dummyBranchingFiltering, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckAC Incremental on a constraint that returns always true with an init removing no value " should " detect no failed test" in {
    val c = new Constraint
    c.gen.setSeed(100)
    var currentVars: Array[Set[Int]] = Array()
    val stack: util.Stack[Array[Set[Int]]] = new util.Stack[Array[Set[Int]]]()

    def dummy(x: Array[Set[Int]]): Array[Set[Int]] = {
      currentVars = dummyConstraint(x)
      currentVars
    }

    def dummyBranchingFiltering(b: BranchOp): Array[Set[Int]] = {
      b match {
        case _: Push => stack.push(currentVars)
        case _: Pop =>
          currentVars = stack.pop()
          currentVars
        case r: RestrictDomain =>
          currentVars = r.applyRestriction
          currentVars
      }
    }

    c.checkAC(dummy, dummyBranchingFiltering, trueChecker)
    assert(c.stats.nbFailedTests == 0)
  }


  "Calling CheckBC for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkBC(dummyConstraint, falseChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckBC for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkBC(throwExceptionConstraint, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckBC for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkBC(noSolutionConstraint, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckBC for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkBC(dummyConstraint, trueChecker)
    assert(c.stats.nbFailedTests == 0)
  }

  "Calling CheckBC with all tests correct " should "perform 100 tests" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.checkBC(dummyConstraint, trueChecker)
    assert(c.stats.getNbExecutedTests == 100)
  }

  "Calling CheckBC after having set a generator " should " consider the good generator " in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.gen.setNbTests(150)
    c.gen.setRangeForAll((-5, 5))
    c.gen.setRange(1, (-2, 2))
    c.gen.setDensity(4, 0.3)
    c.gen.setSeed(125)
    c.checkBC(dummyConstraint, trueChecker)
    assert(c.stats.getGenerator == c.gen)
    assert(c.stats.getNbExecutedTests == 150)
  }

  "Calling CheckBC Incremental on a constraint that returns always true with an init removing some values " should " detect at least an error " in {
    val c = new Constraint
    c.gen.setSeed(100)
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
        case _: Pop =>
          currentVars = stack.pop()
          currentVars
        case r: RestrictDomain =>
          currentVars = r.applyRestriction
          currentVars
      }
    }

    c.checkBC(dummy, dummyBranchingFiltering, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling CheckBC Incremental on a constraint that returns always true with an init removing no value " should " detect no error " in {
    val c = new Constraint
    c.gen.setSeed(100)
    var currentVars: Array[Set[Int]] = Array()
    val stack: util.Stack[Array[Set[Int]]] = new util.Stack[Array[Set[Int]]]()

    def dummy(x: Array[Set[Int]]): Array[Set[Int]] = {
      currentVars = dummyConstraint(x)
      currentVars
    }

    def dummyBranchingFiltering(b: BranchOp): Array[Set[Int]] = {
      b match {
        case _: Push => stack.push(currentVars)
        case _: Pop =>
          currentVars = stack.pop()
          currentVars
        case r: RestrictDomain =>
          currentVars = r.applyRestriction
          currentVars
      }
    }

    c.checkBC(dummy, dummyBranchingFiltering, trueChecker)
    assert(c.stats.nbFailedTests == 0)
  }


  "Calling Check for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect no error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.check(dummyConstraint, falseChecker)
    assert(c.stats.nbFailedTests == 0)
  }

  "Calling Check for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.check(throwExceptionConstraint, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling Check for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "have at least one error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.check(noSolutionConstraint, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling Check for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.check(dummyConstraint, trueChecker)
    assert(c.stats.nbFailedTests == 0)
  }

  "Calling Check with all tests correct " should "perform 100 tests" in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.check(dummyConstraint, trueChecker)
    assert(c.stats.getNbExecutedTests == 100)
  }

  "Calling Check after having set a generator " should " consider the good generator " in {
    val c = new Constraint
    c.gen.setSeed(100)
    c.gen.setNbTests(150)
    c.gen.setRangeForAll((-5, 5))
    c.gen.setRange(1, (-2, 2))
    c.gen.setDensity(4, 0.3)
    c.gen.setSeed(125)
    c.check(dummyConstraint, trueChecker)
    assert(c.stats.getGenerator == c.gen)
    assert(c.stats.getNbExecutedTests == 150)
  }

  "Calling Check Incremental on a constraint that returns always true with an init removing some values " should " detect at least an error " in {
    val c = new Constraint
    c.gen.setSeed(100)
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
        case _: Pop =>
          currentVars = stack.pop()
          currentVars
        case r: RestrictDomain =>
          currentVars = r.applyRestriction
          currentVars
      }
    }

    c.check(dummy, dummyBranchingFiltering, trueChecker)
    assert(c.stats.nbFailedTests > 0)
  }

  "Calling Check Incremental on a constraint that returns always true with an init removing no value " should " detect no error" in {
    val c = new Constraint
    c.gen.setSeed(100)
    var currentVars: Array[Set[Int]] = Array()
    val stack: util.Stack[Array[Set[Int]]] = new util.Stack[Array[Set[Int]]]()

    def dummy(x: Array[Set[Int]]): Array[Set[Int]] = {
      currentVars = dummyConstraint(x)
      currentVars
    }

    def dummyBranchingFiltering(b: BranchOp): Array[Set[Int]] = {
      b match {
        case _: Push => stack.push(currentVars)
        case _: Pop =>
          currentVars = stack.pop()
          currentVars
        case r: RestrictDomain =>
          currentVars = r.applyRestriction
          currentVars
      }
    }

    c.check(dummy, dummyBranchingFiltering, trueChecker)
    assert(c.stats.nbFailedTests == 0)
  }

}

