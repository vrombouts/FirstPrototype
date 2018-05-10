package checker

import java.util

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}
import checker.CPChecker._
import org.scalatest.FlatSpec

/*
 * This class contains the tests for the CheckAC, CheckBC and Check functions
 */
class CheckTests extends FlatSpec {

  val acTrue = new ACFiltering(Checkers.trueConstraint _)
  val acFalse = new ACFiltering(Checkers.falseConstraint _)

  val noSolFilter: Filter = x => Array.fill(x.length)(Set[Int]())
  val throwExceptionFilter: Filter = _ => throw new NoSolutionException
  val dummyFilter: Filter = x => x

  "Calling check with ACFiltering for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(acFalse, dummyFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(acTrue, throwExceptionFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(acTrue, noSolFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check with ACFiltering for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(acTrue, dummyFilter)
    assert(stats.getNbFailedTests == 0)
  }

  "Calling check with ACFiltering with all tests correct " should "perform 100 tests" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(acTrue, dummyFilter)
    assert(stats.getNbExecutedTests == 100)
  }

  "Calling check with ACFiltering after having set a testArguments " should " consider the good testArguments " in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    testArguments.setSeed(100)
    testArguments.setNbTests(150)
    testArguments.setRangeForAll((-5, 5))
    testArguments.setRange(1, (-2, 2))
    testArguments.setDensity(4, 0.3)
    testArguments.setSeed(125)
    stats = new Statistics("")
    check(acTrue, dummyFilter)
    assert(stats.getGenerator == testArguments)
    assert(stats.getNbExecutedTests == 150)
  }

  "Calling check Incremental with ACFiltering on a constraint that returns always true with an init removing some values " should " detect at least a failed test" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    check(acFalse, dummyFilter)
    val dummyInc: FilterWithState = new FilterWithState {
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
        if (variables(4).size > 1) currentVars(4) = variables(4).tail
        currentVars
      }
    }
    val acIncTrue: FilterWithState = new ACFilteringIncremental(Checkers.trueConstraint _)
    stats = new Statistics("")
    check(acIncTrue, dummyInc)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check Incremental with ACFiltering on a constraint that returns always true with an init removing no value " should " detect no failed test" in {
    testArguments = new TestArgs
    testArguments.setSeed(102)
    val dummyInc: FilterWithState = new FilterWithState {
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
    val acIncTrue: FilterWithState = new ACFilteringIncremental(Checkers.trueConstraint _)
    stats = new Statistics("")
    check(acIncTrue, dummyInc)
    assert(stats.getNbFailedTests == 0)
  }


  val bcTrue: Filter = new BCFiltering(Checkers.trueConstraint _)
  val bcFalse: Filter = new BCFiltering(Checkers.falseConstraint _)

  "Calling check with BCFiltering for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(bcFalse, dummyFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check with BCFiltering for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(bcTrue, throwExceptionFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check with BCFiltering for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(bcTrue, noSolFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check with BCFiltering for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(bcTrue, dummyFilter)
    assert(stats.getNbFailedTests == 0)
  }

  "Calling check with BCFiltering with all tests correct " should "perform 100 tests" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    check(bcTrue, dummyFilter)
    assert(stats.getNbExecutedTests == 100)
  }

  "Calling check with BCFiltering after having set a testArguments " should " consider the good testArguments " in {
    testArguments = new TestArgs
    testArguments.setNbTests(150)
    testArguments.setRangeForAll((-5, 5))
    testArguments.setRange(1, (-2, 2))
    testArguments.setDensity(4, 0.3)
    testArguments.setSeed(125)
    stats = new Statistics("")
    check(bcTrue, dummyFilter)
    assert(stats.getGenerator == testArguments)
    assert(stats.getNbExecutedTests == 150)
  }

  "Calling check Incremental with BCFiltering on a constraint that returns always true with an init removing some values " should " detect at least an error " in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    val dummyInc: FilterWithState = new FilterWithState {
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
        if (variables(4).size > 1) currentVars(4) = variables(4).tail
        currentVars
      }
    }
    val bcIncTrue = new BCFilteringIncremental(Checkers.trueConstraint _)
    stats = new Statistics("")
    check(bcIncTrue, dummyInc)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling check Incremental with BCFiltering on a constraint that returns always true with an init removing no value " should " detect no error " in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    val dummyInc: FilterWithState = new FilterWithState {
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
    val bcIncTrue = new BCFilteringIncremental(Checkers.trueConstraint _)
    stats = new Statistics("")
    check(bcIncTrue, dummyInc)
    assert(stats.getNbFailedTests == 0)
  }


  "Calling stronger with ACFiltering for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    stronger(acFalse, dummyFilter)
    assert(stats.getNbFailedTests == 0)
  }

  "Calling stronger with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    stronger(acTrue, throwExceptionFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling stronger with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "have at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    stronger(acTrue, noSolFilter)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling stronger with ACFiltering for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    stronger(acTrue, dummyFilter)
    assert(stats.getNbFailedTests == 0)
  }

  "Calling stronger with ACFiltering with all tests correct " should "perform 100 tests" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    stronger(acTrue, dummyFilter)
    assert(stats.getNbExecutedTests == 100)
  }

  "Calling stronger with ACFiltering after having set a testArguments " should " consider the good testArguments " in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    testArguments.setSeed(100)
    testArguments.setNbTests(150)
    testArguments.setRangeForAll((-5, 5))
    testArguments.setRange(1, (-2, 2))
    testArguments.setDensity(4, 0.3)
    testArguments.setSeed(125)
    stats = new Statistics("")
    stronger(acTrue, dummyFilter)
    assert(stats.getGenerator == testArguments)
    assert(stats.getNbExecutedTests == 150)
  }

  "Calling stronger Incremental with ACFiltering on a constraint that returns always true with an init removing some values " should " detect at least an error " in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    val dummyInc: FilterWithState = new FilterWithState {
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
        if (variables(4).size > 1) currentVars(4) = variables(4).tail
        currentVars
      }
    }
    val acIncTrue = new ACFilteringIncremental(Checkers.trueConstraint _)
    stats = new Statistics("")
    stronger(acIncTrue, dummyInc)
    assert(stats.getNbFailedTests > 0)
  }

  "Calling stronger Incremental with ACFiltering on a constraint that returns always true with an init removing no value " should " detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    stats = new Statistics("")
    val dummyInc: FilterWithState = new FilterWithState {
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
    val acIncTrue = new ACFilteringIncremental(Checkers.trueConstraint _)
    stronger(acIncTrue, dummyInc)
    assert(stats.getNbFailedTests == 0)
  }


}

