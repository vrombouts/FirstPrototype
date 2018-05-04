package checker

import org.scalatest.FlatSpec


class CheckConstraintTests extends FlatSpec {
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
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, dummyFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [0]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, dummyFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, dummyFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [1] [1]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, throwExceptionFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, throwExceptionFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that returns an exception for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, throwExceptionFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns null for domain variables [1] [1]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, nullFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [0,1] [0,1] [0,1]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(0, 1), Set(0, 1), Set(0, 1)), acAllDiff, throwExceptionFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns null for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, nullFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns the initial domains without the first one for domain variables [1] [1]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, badVariablesFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns the initial domains without the first one for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, badVariablesFilter,  new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns all empty domains for domain variables [1] [1]" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, noSolFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns all empty domains for domain variables [1] [0]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(0)), acAllDiff, noSolFilter, new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with a constraint that returns all empty domains for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, noSolFilter,  new StrictStatistics("AC")))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1,0] [0,1] [1,2] considering unstrict format(should not remove solution but does not check that it removes elements that are not solution)" should "return true" in {
    assert(CPChecker.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), acAllDiff, dummyFilter, new UnstrictStats("AC")))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [1] considering unstrict format" should "return false" in {
    assert(!CPChecker.checkConstraint(Array(Set(1), Set(1)), acAllDiff, dummyFilter, new UnstrictStats("AC")))
  }
}
