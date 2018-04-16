package unit.Statistics

import checker.UnstrictStats
import checker.constraints.incremental._
import unit.UnitSpec
import scala.util.Random

class UnstrictStatisticsTests extends UnitSpec {

  val stats = new UnstrictStats

  "incorrectDomains" should "return false if both R1 and R2 possess empty sets" in {
    var R1: Array[Set[Int]] = Array(Set())
    var R2: Array[Set[Int]] = Array(Set())
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set())
    R2 = Array(Set(1), Set())
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set())
    R2 = Array(Set(), Set(2))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(), Set(1))
    R2 = Array(Set(1), Set(), Set(1))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(), Set(1), Set())
    R2 = Array(Set(1), Set(), Set(1))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(), Set(), Set())
    R2 = Array(Set(), Set(), Set())
    assert(!stats.incorrectDomains(R1, R2))
  }

  "incorrectDomains" should "return false if R1 possess an empty set while R2 does not and it has sets bigger than 1" in {
    var R1: Array[Set[Int]] = Array(Set())
    var R2: Array[Set[Int]] = Array(Set(1, 2))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set())
    R2 = Array(Set(1, 2), Set(1))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(), Set())
    R2 = Array(Set(1, 2), Set(1))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set())
    R2 = Array(Set(1, 2), Set(1, 2, 3))
    assert(!stats.incorrectDomains(R1, R2))
  }

  "incorrectDomains" should "return true if R1 possess an empty set while R2 only possess sets of size 1" in {
    var R1: Array[Set[Int]] = Array(Set())
    var R2: Array[Set[Int]] = Array(Set(1))
    assert(stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set())
    R2 = Array(Set(1), Set(1))
    assert(stats.incorrectDomains(R1, R2))
    R1 = Array(Set(), Set())
    R2 = Array(Set(1), Set(2))
    assert(stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set())
    R2 = Array(Set(2), Set(1))
    assert(stats.incorrectDomains(R1, R2))
  }

  "incorrectDomains" should "return false if R1 does not have an empty set and R1 is included in R2" in {
    var R1: Array[Set[Int]] = Array(Set(1))
    var R2: Array[Set[Int]] = Array(Set(1))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1))
    R2 = Array(Set(1, 2))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1))
    R2 = Array(Set(1), Set(1))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1))
    R2 = Array(Set(1), Set(1, 2))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1))
    R2 = Array(Set(1, 2), Set(1, 2))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1, 2))
    R2 = Array(Set(1), Set(1, 2))
    assert(!stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1, 2))
    R2 = Array(Set(1), Set(1, 2, 3))
    assert(!stats.incorrectDomains(R1, R2))
  }

  "incorrectDomains" should "return true if R1 is not included in R2 and possess no empty sets" in {
    var R1 = Array(Set(1))
    var R2 = Array(Set(2))
    assert(stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1))
    R2 = Array(Set(1), Set(2))
    assert(stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1, 2))
    R2 = Array(Set(1, 2), Set(1))
    assert(stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1, 2))
    R2 = Array(Set(1), Set(1))
    assert(stats.incorrectDomains(R1, R2))
    R1 = Array(Set(1), Set(1, 3))
    R2 = Array(Set(1), Set(1, 2))
    assert(stats.incorrectDomains(R1, R2))
  }

  "strictDomainComparison" should "increment the returned value of nbFailTests if R1 is not included in R2" in {
    var R1 = Array(Set(1))
    var R2 = Array(Set(2))
    var f = stats.nbFailedTests
    stats.strictDomainComparison(R1, R2, null, false)
    stats.incNbExecutedTests()
    assert(f + 1 == stats.nbFailedTests)
    R1 = Array(Set(1), Set(1))
    R2 = Array(Set(1), Set(2))
    f = stats.nbFailedTests
    stats.incNbExecutedTests()
    stats.strictDomainComparison(R1, R2, null, false)
    assert(f + 1 == stats.nbFailedTests)
    R1 = Array(Set(1), Set(1, 2))
    R2 = Array(Set(1, 2), Set(1))
    f = stats.nbFailedTests
    stats.incNbExecutedTests()
    stats.strictDomainComparison(R1, R2, null, false)
    assert(f + 1 == stats.nbFailedTests)
    R1 = Array(Set(1), Set(1, 2))
    R2 = Array(Set(1), Set(1))
    f = stats.nbFailedTests
    stats.incNbExecutedTests()
    stats.strictDomainComparison(R1, R2, null, false)
    assert(f + 1 == stats.nbFailedTests)
    R1 = Array(Set(1), Set(1, 3))
    R2 = Array(Set(1), Set(1, 2))
    f = stats.nbFailedTests
    stats.incNbExecutedTests()
    stats.strictDomainComparison(R1, R2, null, false)
    assert(f + 1 == stats.nbFailedTests)
  }

  "strictDomainComparison" should "increment nbCorrectTestWithSolution if R1==R2" in {
    var R1 = Array(Set(1))
    var R2 = Array(Set(1))
    var n = stats.getNbCorrectTestsWithSolution
    stats.strictDomainComparison(R1, R2, null, false)
    assert(n + 1 == stats.getNbCorrectTestsWithSolution)
    R1 = Array(Set(1), Set(1, 2))
    R2 = Array(Set(1), Set(1, 2))
    n = stats.getNbCorrectTestsWithSolution
    stats.strictDomainComparison(R1, R2, null, false)
    assert(n + 1 == stats.getNbCorrectTestsWithSolution)
    R1 = Array(Set(1, 3, 2))
    R2 = Array(Set(1, 2, 3))
    n = stats.getNbCorrectTestsWithSolution
    stats.strictDomainComparison(R1, R2, null, false)
    assert(n + 1 == stats.getNbCorrectTestsWithSolution)
  }
  "strictDomainComparison" should "increment nbCorrectTestWithSolution and canBeMoreFiltered if R1 included in R2" in {
    var R1 = Array(Set(1))
    var R2 = Array(Set(1, 2))
    var n = stats.getNbCorrectTestsWithSolution
    var c = stats.getCanBeMoreFiltered
    stats.strictDomainComparison(R1, R2, null, false)
    assert(n + 1 == stats.getNbCorrectTestsWithSolution)
    assert(c + 1 == stats.getCanBeMoreFiltered)
    R1 = Array(Set(1), Set(1))
    R2 = Array(Set(1, 2), Set(1))
    n = stats.getNbCorrectTestsWithSolution
    c = stats.getCanBeMoreFiltered
    stats.strictDomainComparison(R1, R2, null, false)
    assert(n + 1 == stats.getNbCorrectTestsWithSolution)
    assert(c + 1 == stats.getCanBeMoreFiltered)
    R1 = Array(Set(1), Set(1))
    R2 = Array(Set(1, 2), Set(1, 2))
    n = stats.getNbCorrectTestsWithSolution
    c = stats.getCanBeMoreFiltered
    stats.strictDomainComparison(R1, R2, null, false)
    assert(n + 1 == stats.getNbCorrectTestsWithSolution)
    assert(c + 1 == stats.getCanBeMoreFiltered)
  }
}