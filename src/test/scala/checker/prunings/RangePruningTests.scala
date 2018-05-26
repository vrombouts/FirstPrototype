package checker.prunings

import java.util.function.Function

import checker.{Checkers, NoSolutionException}
import org.scalatest.FlatSpec

class RangePruningTests extends FlatSpec {

  val allDifferentFilter = new RangePruning(Checkers.allDifferent())

  val allDifJavaChecker: Function[Array[Integer], java.lang.Boolean] = {
    domains => {
      var result: Boolean = true
      for (i <- domains.indices) {
        for (j <- 0 until i) {
          if (domains(j).equals(domains(i))) {
            result = false
          }
        }
      }
      result
    }
  }

  val allDiffJava: RangePruning = new RangePruning(allDifJavaChecker)

  "Calling the filter of RCPruning for AllDifferent on domains [1] [1]" should "return an exception" in {
    assertThrows[NoSolutionException] {
      allDifferentFilter.filter(Array(Set(1), Set(1)))
    }
    assertThrows[NoSolutionException] {
      allDiffJava.filter(Array(Set(1), Set(1)))
    }
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,1] [0,1]" should "return domains [0,1] [0,1]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1), Set(0, 1)))
    val b: Array[Set[Int]] = Array(Set(0, 1), Set(0, 1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1), Set(0, 1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,1] [0,1] [0,1]" should "return an exception" in {
    assertThrows[NoSolutionException] {
      allDifferentFilter.filter(Array(Set(0, 1), Set(0, 1), Set(0, 1)))
    }
    assertThrows[NoSolutionException] {
      allDiffJava.filter(Array(Set(0, 1), Set(0, 1), Set(0, 1)))
    }
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,1] [1]" should "return domains [0] [1]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1), Set(1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,1,2] [2,3] [1]" should "return domains [0,2] [2,3] [1]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1, 2), Set(2, 3), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0, 2), Set(2, 3), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDifferentFilter.filter(Array(Set(0, 1, 2), Set(2, 3), Set(1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,1,2] [1,2] [2,3] [0]" should "return domains [1,2] [1,2] [3] [0]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1, 2), Set(1, 2), Set(2, 3), Set(0)))
    val b: Array[Set[Int]] = Array(Set(1, 2), Set(1, 2), Set(3), Set(0))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1, 2), Set(1, 2), Set(2, 3), Set(0)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of RCPruning for AllDifferent on domains [1,2,3] [0,5] [1,5,6] [1,2] [2,3] [0]" should "return domains [1,2,3] [5] [6] [1,2] [2,3] [0]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(1, 2, 3), Set(0, 5), Set(1, 5, 6), Set(1, 2), Set(2, 3), Set(0)))
    val b: Array[Set[Int]] = Array(Set(1, 2, 3), Set(5), Set(6), Set(1, 2), Set(2, 3), Set(0))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(1, 2, 3), Set(0, 5), Set(1, 5, 6), Set(1, 2), Set(2, 3), Set(0)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,3,4] [1,3], [4], [1]" should "return domains [0] [3] [4] [1] " in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 3, 4), Set(1, 3), Set(4), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0), Set(3), Set(4), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 3, 4), Set(1, 3), Set(4), Set(1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,3,6,8,10] [0,1] [10] [1] [6]" should "return domains [3,8] [0] [10] [1] [6] " in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 3, 6, 8, 10), Set(0, 1), Set(10), Set(1), Set(6)))
    val b: Array[Set[Int]] = Array(Set(3, 8), Set(0), Set(10), Set(1), Set(6))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 3, 6, 8, 10), Set(0, 1), Set(10), Set(1), Set(6)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of RCPruning for AllDifferent on domains [0,1,2,4] [1,2,4] [1,2,4] [1,2,4]" should "return domains [0,1,2,4] [1,2,4] [1,2,4] [1,2,4] " in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1, 2, 4), Set(1, 2, 4), Set(1, 2, 4), Set(1, 2, 4)))
    val b: Array[Set[Int]] = Array(Set(0, 1, 2, 4), Set(1, 2, 4), Set(1, 2, 4), Set(1, 2, 4))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1, 2, 4), Set(1, 2, 4), Set(1, 2, 4), Set(1, 2, 4)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "calling the filter of RCPruning for AllDifferent on domain [1]" should "return [1]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(1)))
    val b: Array[Set[Int]] = Array(Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "calling the filter of RCPruning for false constraint on domain [1,2,3]" should "throw a noSolutionException" in {
    val falseChecker: Array[Int] => Boolean = _ => false
    val falseFilter = new RangePruning(falseChecker)
    assertThrows[NoSolutionException] {
      falseFilter.filter(Array(Set(1, 2, 3)))
    }
  }

  "calling filter of RCPruning with a checker that confirm the length of the solution is equal to the number of variables" should "return false always since it prune during the search with the checker" in {
    val lengthChecker: Array[Int] => Boolean = x => x.length == 4
    val falseFilter = new RangePruning(lengthChecker)
    assertThrows[NoSolutionException] {
      falseFilter.filter(Array(Set(1), Set(1), Set(1), Set(1)))
    }
  }
}


