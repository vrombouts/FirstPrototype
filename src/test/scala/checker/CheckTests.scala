package checker

import java.util

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}
import checker.CPChecker._
import checker.statistics.{CheckStatistics, StrongerStatistics}
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
    checkStats = new CheckStatistics("")
    check(acFalse, dummyFilter)
    assert(checkStats.nbFailedTests > 0)
  }

  "Calling check with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(acTrue, throwExceptionFilter)
    assert(checkStats.nbFailedTests > 0)
  }

  "Calling check with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(acTrue, noSolFilter)
    assert(checkStats.nbFailedTests > 0)
  }

  "Calling check with ACFiltering for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(acTrue, dummyFilter)
    assert(checkStats.nbFailedTests == 0)
  }

  "Calling check with ACFiltering with all tests correct " should "perform 100 tests" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(acTrue, dummyFilter)
    assert(checkStats.getNbExecutedTests == 100)
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
    checkStats = new CheckStatistics("")
    check(acTrue, dummyFilter)
    assert(checkStats.getGenerator == testArguments)
    assert(checkStats.getNbExecutedTests == 150)
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
    checkStats = new CheckStatistics("")
    check(acIncTrue, dummyInc)
    assert(checkStats.nbFailedTests > 0)
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
    checkStats = new CheckStatistics("")
    check(acIncTrue, dummyInc)
    assert(checkStats.nbFailedTests == 0)
  }


  val bcTrue: Filter = new BCFiltering(Checkers.trueConstraint _)
  val bcFalse: Filter = new BCFiltering(Checkers.falseConstraint _)

  "Calling check with BCFiltering for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(bcFalse, dummyFilter)
    assert(checkStats.nbFailedTests > 0)
  }

  "Calling check with BCFiltering for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(bcTrue, throwExceptionFilter)
    assert(checkStats.nbFailedTests > 0)
  }

  "Calling check with BCFiltering for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(bcTrue, noSolFilter)
    assert(checkStats.nbFailedTests > 0)
  }

  "Calling check with BCFiltering for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(bcTrue, dummyFilter)
    assert(checkStats.nbFailedTests == 0)
  }

  "Calling check with BCFiltering with all tests correct " should "perform 100 tests" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    checkStats = new CheckStatistics("")
    check(bcTrue, dummyFilter)
    assert(checkStats.getNbExecutedTests == 100)
  }

  "Calling check with BCFiltering after having set a testArguments " should " consider the good testArguments " in {
    testArguments = new TestArgs
    testArguments.setNbTests(150)
    testArguments.setRangeForAll((-5, 5))
    testArguments.setRange(1, (-2, 2))
    testArguments.setDensity(4, 0.3)
    testArguments.setSeed(125)
    checkStats = new CheckStatistics("")
    check(bcTrue, dummyFilter)
    assert(checkStats.getGenerator == testArguments)
    assert(checkStats.getNbExecutedTests == 150)
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
    checkStats = new CheckStatistics("")
    check(bcIncTrue, dummyInc)
    assert(checkStats.nbFailedTests > 0)
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
    checkStats = new CheckStatistics("")
    check(bcIncTrue, dummyInc)
    assert(checkStats.nbFailedTests == 0)
  }


  "Calling stronger with ACFiltering for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    strongerStats = new StrongerStatistics("")
    stronger(acFalse, dummyFilter)
    assert(strongerStats.nbFailedTests == 0)
  }

  "Calling stronger with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    strongerStats = new StrongerStatistics("")
    stronger(acTrue, throwExceptionFilter)
    assert(strongerStats.nbFailedTests > 0)
  }

  "Calling stronger with ACFiltering for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "have at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    strongerStats = new StrongerStatistics("")
    stronger(acTrue, noSolFilter)
    assert(strongerStats.nbFailedTests > 0)
  }

  "Calling stronger with ACFiltering for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    strongerStats = new StrongerStatistics("")
    stronger(acTrue, dummyFilter)
    assert(strongerStats.nbFailedTests == 0)
  }

  "Calling stronger with ACFiltering with all tests correct " should "perform 100 tests" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    strongerStats = new StrongerStatistics("")
    stronger(acTrue, dummyFilter)
    assert(strongerStats.getNbExecutedTests == 100)
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
    strongerStats = new StrongerStatistics("")
    stronger(acTrue, dummyFilter)
    assert(strongerStats.getGenerator == testArguments)
    assert(strongerStats.getNbExecutedTests == 150)
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
    strongerStats = new StrongerStatistics("")
    stronger(acIncTrue, dummyInc)
    assert(strongerStats.nbFailedTests > 0)
  }

  "Calling stronger Incremental with ACFiltering on a constraint that returns always true with an init removing no value " should " detect no error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    strongerStats = new StrongerStatistics("")
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
    assert(strongerStats.nbFailedTests == 0)
  }

}

