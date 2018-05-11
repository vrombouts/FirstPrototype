package checker

import java.util

import checker.CPChecker._
import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}
import org.scalatest.FlatSpec

class CPCheckerTests extends FlatSpec {

  //TESTS of the comparison functions

  "comparisonCheck" should "return true if v(1) is the same as v(2)" in {
    var v = Array(
      Array(Set(1)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1), Set(2)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1)),
      Array(Set(1), Set(2)),
      Array(Set(1), Set(2))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(),
      Array(),
      Array()
    )
    assert(CPChecker.comparisonCheck(v))
  }
  //only testing if all elements of v have the same length (pre-condition)
  "comparisonCheck" should "return false if v(1) is included in v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set()),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3)),
      Array(Set(1, 2), Set(1, 2, 3))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(2) is included in v(1)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set(1))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set())
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(1) and v(2) included in v(0) and v(1)!=v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2, 3)),
      Array(Set(2, 1)),
      Array(Set(2, 3))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(1) is null" in {
    val v = Array(
      Array(Set(1, 2)),
      null,
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(1)'s length is different than v(2)'s length" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1), Set(1)),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2)),
      Array(),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return true if v(1) and v(2) both possess an empty set" in {
    var v: Array[Array[Set[Int]]] = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(1), Set()),
      Array(Set(1), Set())
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(2), Set()),
      Array(Set(1), Set())
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(2), Set()),
      Array(Set(), Set(2))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(), Set()),
      Array(Set(1), Set())
    )
    assert(CPChecker.comparisonCheck(v))
  }

  "comparisonStronger" should "return true if v(1) is the same as v(2)" in {
    var v = Array(
      Array(Set(1)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1), Set(2)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1)),
      Array(Set(1), Set(2)),
      Array(Set(1), Set(2))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(),
      Array(),
      Array()
    )
    assert(CPChecker.comparisonStronger(v))
  }
  //only testing if all elements of v have the same length (pre-condition)
  "comparisonStronger" should "return false if v(1) is included in v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set()),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3)),
      Array(Set(1, 2), Set(1, 2, 3))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2))
    )
    assert(!CPChecker.comparisonStronger(v))
  }

  "comparisonStronger" should "return true if v(2) is included in v(1)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set())
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2))
    )
    assert(CPChecker.comparisonStronger(v))
  }

  "comparisonStronger" should "return false if v(1) and v(2) included in v(0) and v(1)!=v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2, 3)),
      Array(Set(2, 1)),
      Array(Set(2, 3))
    )
    assert(!CPChecker.comparisonStronger(v))
  }


  // tests for the checkconstraint function

  val noSolFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = Array.fill(variables.length)(Set[Int]())
  }
  val badVariablesFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = variables.tail
  }
  val nullFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = null
  }
  val throwExceptionFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = throw new NoSolutionException
  }
  val dummyFilter: Filter = new Filter {
    override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = variables
  }

  val acAllDiff = new ACFiltering(Checkers.allDifferent())

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [1]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, dummyFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [0]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, dummyFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, dummyFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [1] [1]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, throwExceptionFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, throwExceptionFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that returns an exception for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, throwExceptionFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns null for domain variables [1] [1]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, nullFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [0,1] [0,1] [0,1]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(0, 1), Set(0, 1), Set(0, 1)), acAllDiff, throwExceptionFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns null for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, nullFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns the initial domains without the first one for domain variables [1] [1]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, badVariablesFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns the initial domains without the first one for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, badVariablesFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns all empty domains for domain variables [1] [1]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, noSolFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns all empty domains for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, noSolFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with a constraint that returns all empty domains for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, noSolFilter, CPChecker.comparisonCheck(_)))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1,0] [0,1] [1,2] considering unstrict format(should not remove solution but does not check that it removes elements that are not solution)" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, dummyFilter, CPChecker.comparisonStronger(_)))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [1] considering unstrict format" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, dummyFilter, CPChecker.comparisonStronger(_)))
  }

  // tests checkConstraint incremental

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
  implicit var testArguments: TestArgs = new TestArgs
  testArguments.setSeed(100)
  testArguments.setNbTests(1)
  //set seed of CPChecker
  "checkConstraint with an empty domain or empty" should "return false if init return non-empty domains" in {
    val inc: FilterWithState = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set(1))
    }
    val a: Array[Set[Int]] = Array()
    val b: Array[Set[Int]] = Array(Set())

    assert(!CPChecker.checkConstraint(a, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
    assert(!CPChecker.checkConstraint(b, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
  }

  "checkConstraint with an empty domain or empty" should "return true if init return empty or empty domains" in {
    val inc: FilterWithState = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array()
    }
    val inc2: FilterWithState = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set())
    }
    val a: Array[Set[Int]] = Array()
    val b: Array[Set[Int]] = Array(Set())
    assert(CPChecker.checkConstraint(a, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
    assert(CPChecker.checkConstraint(b, dummyInc, inc2, CPChecker.comparisonCheck(_, _)))
  }

  "CheckConstraint with init returning an array of different size" should "be false" in {
    val inc: FilterWithState = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set(5), Set(5))
    }
    val inc2: FilterWithState = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array(Set(5))
    }
    val inc3: FilterWithState = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = Array()
    }
    val a: Array[Set[Int]] = Array()
    assert(!CPChecker.checkConstraint(Array(Set(5)), dummyInc, inc, CPChecker.comparisonCheck(_, _)))
    assert(!CPChecker.checkConstraint(Array(Set(5), Set(5)), dummyInc, inc2, CPChecker.comparisonCheck(_, _)))
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
    assert(!CPChecker.checkConstraint(Array(Set(5)), dummyInc, inc3, CPChecker.comparisonCheck(_, _)))
  }

  "CheckConstraint with applyConstraint as init and filtering" should "always be true" in {
    val a: Array[Set[Int]] = Array()
    val b: Array[Set[Int]] = Array(Set())
    val c: Array[Set[Int]] = Array(Set(1, 4, 5), Set())
    assert(CPChecker.checkConstraint(a, dummyInc, dummyInc, CPChecker.comparisonCheck(_, _)))
    assert(CPChecker.checkConstraint(b, dummyInc, dummyInc, CPChecker.comparisonCheck(_, _)))
    assert(CPChecker.checkConstraint(Array(Set(1)), dummyInc, dummyInc, CPChecker.comparisonCheck(_, _)))
    assert(CPChecker.checkConstraint(Array(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3)), dummyInc, dummyInc, CPChecker.comparisonCheck(_, _)))
    assert(CPChecker.checkConstraint(c, dummyInc, dummyInc, CPChecker.comparisonCheck(_, _)))
    assert(CPChecker.checkConstraint(Array(Set(1, 4, 5, 8, 7), Set(1, 4, 5, 8, 7, 9)), dummyInc, dummyInc, CPChecker.comparisonCheck(_, _)))
  }

  "checkConstraint with applyConstraint as init and a dummy filtering" should "return false because of filtering" in {
    var i = 0
    val a = Array(Set(1, 2), Set(1, 2))
    val inc = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = {
        if (branching.isInstanceOf[RestrictDomain]) i += 1
        a
      }

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = dummyInc.setup(variables)
    }
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
    assert(i == 1)
  }

  "checkConstraint with init throwing an exception" should "consider it as a NoSolutionException" in {
    val a = Array(Set(1), Set(2))
    val b: Array[Set[Int]] = Array(Set())
    val inc = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = Array()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = throw new Exception()
    }
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
    assert(CPChecker.checkConstraint(b, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
  }

  "checkConstraint with filtering throwing an exception" should "consider it as a NoSolutionException" in {
    val a = Array(Set(1, 2), Set(1, 2))
    val inc = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = throw new Exception()

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = dummyInc.setup(variables)
    }
    assert(!CPChecker.checkConstraint(a, dummyInc, inc, CPChecker.comparisonCheck(_, _)))
  }

  // tests for the check and stronger functions

  val acTrue = new ACFiltering(Checkers.trueConstraint _)
  val acFalse = new ACFiltering(Checkers.falseConstraint _)

  "Calling check with ACFiltering for checking a constraint returning always false with a checker returning false and a filtering process that does nothing" should "detect at least one error" in {
    testArguments = new TestArgs
    testArguments.setSeed(100)
    val stats = new Statistics("")
    check(acFalse, dummyFilter)(testArguments, stats)
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
    val acIncTrue: FilterWithState = new IncrementalFiltering(new ACFiltering(Checkers.trueConstraint _))
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
    val acIncTrue: FilterWithState = new IncrementalFiltering(new ACFiltering(Checkers.trueConstraint _))
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
    val bcIncTrue = new IncrementalFiltering( new BCFiltering(Checkers.trueConstraint _))
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
    val bcIncTrue = new IncrementalFiltering( new BCFiltering(Checkers.trueConstraint _))
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
    val acIncTrue = new IncrementalFiltering(new ACFiltering(Checkers.trueConstraint _))
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
    val acIncTrue = new IncrementalFiltering(new ACFiltering(Checkers.trueConstraint _))
    stronger(acIncTrue, dummyInc)
    assert(stats.getNbFailedTests == 0)
  }


}
