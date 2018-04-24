package unit

import checker.{NoSolutionException, StrictStatistics, UnstrictStats}
import checker.constraints.AllDifferent

class CheckConstraintTests extends UnitSpec {

  def dummyConstraint(x: Array[Set[Int]]): Array[Set[Int]] = x

  @throws[NoSolutionException]
  def throwExceptionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = throw new NoSolutionException

  def nullConstraint(x: Array[Set[Int]]): Array[Set[Int]] = null

  def badVariablesConstraint(x: Array[Set[Int]]): Array[Set[Int]] = x.tail

  def noSolutionConstraint(x: Array[Set[Int]]): Array[Set[Int]] = Array.fill(x.length)(Set[Int]())

  // add a constraint that makes the allDifferent constraint but only one time (no fix point reached)

  val c: AllDifferent = new AllDifferent {
    propagation=AC
    stats = new StrictStatistics("AC")
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [1]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(1)), dummyConstraint))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [0]" should "return true" in {
    assert(c.checkConstraint(Array(Set(1), Set(0)), dummyConstraint))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), dummyConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [1] [1]" should "return true" in {
    assert(c.checkConstraint(Array(Set(1), Set(1)), throwExceptionConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [1] [0]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(0)), throwExceptionConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that returns an exception for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), throwExceptionConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns null for domain variables [1] [1]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(1)), nullConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns an exception for domain variables [0,1] [0,1] [0,1]" should "return true" in {
    assert(c.checkConstraint(Array(Set(0, 1), Set(0, 1), Set(0, 1)), throwExceptionConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns null for domain variables [1] [0]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(0)), nullConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns the initial domains without the first one for domain variables [1] [1]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(1)), badVariablesConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns the initial domains without the first one for domain variables [1] [0]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(0)), badVariablesConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns all empty domains for domain variables [1] [1]" should "return true" in {
    assert(c.checkConstraint(Array(Set(1), Set(1)), noSolutionConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that simply returns all empty domains for domain variables [1] [0]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(0)), noSolutionConstraint))
  }

  "Comparing the allDifferent constraint with a constraint that returns all empty domains for domain variables [1,0] [0,1] [1,2]" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), noSolutionConstraint))
  }


  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1,0] [0,1] [1,2] considering unstrict format(should not remove solution but does not check that it removes elements that are not solution)" should "return true" in {
    c.stats = new UnstrictStats(null)
    assert(c.checkConstraint(Array(Set(1, 0), Set(0, 1), Set(1, 2)), dummyConstraint))
  }

  "Comparing the allDifferent constraint with the constraint that does nothing for domain variables [1] [1] considering unstrict format" should "return false" in {
    assert(!c.checkConstraint(Array(Set(1), Set(1)), dummyConstraint))
  }
}
