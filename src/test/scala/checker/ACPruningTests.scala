package checker

import java.util.function.Function

import org.scalatest.FlatSpec

class ACPruningTests extends FlatSpec {

  val allDifferentFilter: Filter = new ACPruning(Checkers.allDifferent())

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

  val allDiffJava: ACPruning = new ACPruning(allDifJavaChecker)

  "Calling the filter of ACPruning for AllDifferent on domains [1] [1]" should "return an exception" in {
    assertThrows[NoSolutionException] {
      allDifferentFilter.filter(Array(Set(1), Set(1)))
    }
    assertThrows[NoSolutionException] {
      allDiffJava.filter(Array(Set(1), Set(1)))
    }
  }

  "Calling the filter of ACPruning for AllDifferent on domains [0,1] [0,1]" should "return domains [0,1] [0,1]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1), Set(0, 1)))
    val b: Array[Set[Int]] = Array(Set(0, 1), Set(0, 1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1), Set(0, 1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of ACPruning for AllDifferent on domains [0,1] [0,1] [0,1]" should "return an exception" in {
    assertThrows[NoSolutionException] {
      allDifferentFilter.filter(Array(Set(0, 1), Set(0, 1), Set(0, 1)))
    }
    assertThrows[NoSolutionException] {
      allDiffJava.filter(Array(Set(0, 1), Set(0, 1), Set(0, 1)))
    }
  }

  "Calling the filter of ACPruning for AllDifferent on domains [0,1] [1]" should "return domains [0] [1]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1), Set(1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of ACPruning for AllDifferent on domains [0,1,2] [2,3] [1]" should "return domains [0,2] [2,3] [1]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1, 2), Set(2, 3), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0, 2), Set(2, 3), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1, 2), Set(2, 3), Set(1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))

  }

  "Calling the filter of ACpruning for AllDifferent on domains [0,1,2] [1,2] [2,3] [0]" should "return domains [1,2] [1,2] [3] [0]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 1, 2), Set(1, 2), Set(2, 3), Set(0)))
    val b: Array[Set[Int]] = Array(Set(1, 2), Set(1, 2), Set(3), Set(0))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 1, 2), Set(1, 2), Set(2, 3), Set(0)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of ACPruning for AllDifferent on domains [1,2,3] [0,5] [1,5,6] [1,2] [2,3] [0]" should "return domains [1,2,3] [5] [6] [1,2] [2,3] [0]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(1, 2, 3), Set(0, 5), Set(1, 5, 6), Set(1, 2), Set(2, 3), Set(0)))
    val b: Array[Set[Int]] = Array(Set(1, 2, 3), Set(5), Set(6), Set(1, 2), Set(2, 3), Set(0))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(1, 2, 3), Set(0, 5), Set(1, 5, 6), Set(1, 2), Set(2, 3), Set(0)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of ACPruning for AllDifferent on domains [0,3,4] [1,3] [4] [1]" should "return domains [0] [3] [4] [1] " in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 3, 4), Set(1, 3), Set(4), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0), Set(3), Set(4), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 3, 4), Set(1, 3), Set(4), Set(1)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of ACPruning for AllDifferent on domains [0,3,6,8,10] [0,1] [10] [1] [6]" should "return domains [3,8] [0,1] [10] [1] [6] " in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(0, 3, 6, 8, 10), Set(0, 1), Set(10), Set(1), Set(6)))
    val b: Array[Set[Int]] = Array(Set(3, 8), Set(0), Set(10), Set(1), Set(6))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(0, 3, 6, 8, 10), Set(0, 1), Set(10), Set(1), Set(6)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of ACPruning for AllDifferent on domain [1,2]" should "return [1,2]" in {
    var a: Array[Set[Int]] = allDifferentFilter.filter(Array(Set(1, 2)))
    val b: Array[Set[Int]] = Array(Set(1, 2))
    assert((a zip b).forall(x => x._1.equals(x._2)))
    a = allDiffJava.filter(Array(Set(1, 2)))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling the filter of ACPruning on empty domain" should "throw a NoSolutionException" in {
    assertThrows[NoSolutionException] {
      allDifferentFilter.filter(Array())
    }
    assertThrows[NoSolutionException] {
      allDiffJava.filter(Array())
    }
  }
}
