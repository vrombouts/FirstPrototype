package unit

import java.util

import checker._
import checker.constraints.Constraint
import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}

/*
 * This class contains the tests for the CheckAC, CheckBC and Check functions
 */
class CheckTests extends UnitSpec {

  val acTrue  = new ACFiltering(Checkers.trueConstraint _)
  val acFalse = new ACFiltering(Checkers.falseConstraint _)

  val noSolFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
      Array.fill(variables.length)(Set[Int]())
    }
  }
  val throwExceptionFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = throw new NoSolutionException
  }
  val dummyFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = variables
  }

  "Calling CheckAC for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acFalse, dummyFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acTrue, throwExceptionFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acTrue, noSolFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckAC for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acTrue, dummyFilter)
    assert(CPChecker.stats.nbFailedTests == 0)
  }

  "Calling CheckAC with all tests correct " should "perform 100 tests" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acTrue, dummyFilter)
    assert(CPChecker.stats.getNbExecutedTests == 100)
  }

  "Calling CheckAC after having set a generator " should " consider the good generator " in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    generator.setSeed(100)
    generator.setNbTests(150)
    generator.setRangeForAll((-5, 5))
    generator.setRange(1, (-2, 2))
    generator.setDensity(4, 0.3)
    generator.setSeed(125)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acTrue, dummyFilter)
    assert(CPChecker.stats.getGenerator == generator)
    assert(CPChecker.stats.getNbExecutedTests == 150)
  }

  "Calling CheckAC Incremental on a constraint that returns always true with an init removing some values " should " detect at least a failed test" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.check(acFalse, dummyFilter)
    val dummyInc :FilterWithState = new FilterWithState {
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
    val acIncTrue:FilterWithState = new ACFilteringIncremental(Checkers.trueConstraint _)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acIncTrue, dummyInc)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckAC Incremental on a constraint that returns always true with an init removing no value " should " detect no failed test" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    val dummyInc :FilterWithState = new FilterWithState {
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
    val acIncTrue:FilterWithState = new ACFilteringIncremental(Checkers.trueConstraint _)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(acIncTrue, dummyInc)
    assert(CPChecker.stats.nbFailedTests == 0)
  }


  val bcTrue : Filter = new BCFiltering(Checkers.trueConstraint _)
  val bcFalse: Filter = new BCFiltering(Checkers.falseConstraint _)

  "Calling CheckBC for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcFalse, dummyFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckBC for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcTrue , throwExceptionFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckBC for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "detect at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcTrue, noSolFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckBC for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcTrue, dummyFilter)
    assert(CPChecker.stats.nbFailedTests == 0)
  }

  "Calling CheckBC with all tests correct " should "perform 100 tests" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcTrue, dummyFilter)
    assert(CPChecker.stats.getNbExecutedTests == 100)
  }

  "Calling CheckBC after having set a generator " should " consider the good generator " in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setNbTests(150)
    generator.setRangeForAll((-5, 5))
    generator.setRange(1, (-2, 2))
    generator.setDensity(4, 0.3)
    generator.setSeed(125)
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcTrue, dummyFilter)
    assert(CPChecker.stats.getGenerator == generator)
    assert(CPChecker.stats.getNbExecutedTests == 150)
  }

  "Calling CheckBC Incremental on a constraint that returns always true with an init removing some values " should " detect at least an error " in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    val dummyInc :FilterWithState = new FilterWithState {
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
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcIncTrue, dummyInc)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling CheckBC Incremental on a constraint that returns always true with an init removing no value " should " detect no error " in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    val dummyInc :FilterWithState = new FilterWithState {
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
    CPChecker.stats = new StrictStatistics(20,"AC")
    CPChecker.check(bcIncTrue, dummyInc)
    assert(CPChecker.stats.nbFailedTests == 0)
  }


  "Calling Check for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect no error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new UnstrictStats(20,"AC")
    CPChecker.stronger(acFalse, dummyFilter)
    assert(CPChecker.stats.nbFailedTests == 0)
  }

  "Calling Check for checking a constraint returning always true with a checker returning true and a filtering that always throws an error" should "detect at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new UnstrictStats(20,"AC")
    CPChecker.stronger(acTrue, throwExceptionFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling Check for checking a constraint returning always true with a checker returning true and a filtering that always returns empty domains" should "have at least one error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new UnstrictStats(20,"AC")
    CPChecker.stronger(acTrue, noSolFilter)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling Check for checking an constraint that does nothing with a correct checker (returning always true) and a filtering that always returns empty domains" should "detect no error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new UnstrictStats(20,"AC")
    CPChecker.stronger(acTrue, dummyFilter)
    assert(CPChecker.stats.nbFailedTests == 0)
  }

  "Calling Check with all tests correct " should "perform 100 tests" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new UnstrictStats(20,"AC")
    CPChecker.stronger(acTrue, dummyFilter)
    assert(CPChecker.stats.getNbExecutedTests == 100)
  }

  "Calling Check after having set a generator " should " consider the good generator " in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    generator.setSeed(100)
    generator.setNbTests(150)
    generator.setRangeForAll((-5, 5))
    generator.setRange(1, (-2, 2))
    generator.setDensity(4, 0.3)
    generator.setSeed(125)
    CPChecker.stats = new UnstrictStats(20,"AC")
    CPChecker.stronger(acTrue, dummyFilter)
    assert(CPChecker.stats.getGenerator == generator)
    assert(CPChecker.stats.getNbExecutedTests == 150)
  }

  "Calling Check Incremental on a constraint that returns always true with an init removing some values " should " detect at least an error " in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    val dummyInc :FilterWithState = new FilterWithState {
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
    CPChecker.stats = new UnstrictStats(20,"AC")
    CPChecker.stronger(acIncTrue, dummyInc)
    assert(CPChecker.stats.nbFailedTests > 0)
  }

  "Calling Check Incremental on a constraint that returns always true with an init removing no value " should " detect no error" in {
    implicit val generator: VariablesGenerator = new VariablesGenerator
    generator.setSeed(100)
    CPChecker.stats = new UnstrictStats(20,"AC")
    val dummyInc :FilterWithState = new FilterWithState {
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
    CPChecker.stronger(acIncTrue, dummyInc)
    assert(CPChecker.stats.nbFailedTests == 0)
  }

}

