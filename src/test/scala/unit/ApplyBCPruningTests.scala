package unit

import checker.NoSolutionException
import checker.constraints.{AllDifferent, Constraint, PruningConstraint}

class ApplyBCPruningTests extends UnitSpec {

  val C = new AllDifferent

  "Calling applyBCPruning for AllDifferent on domains [1] [1]" should "return an exception" in {
    assertThrows[NoSolutionException] {
      C.applyBCPruning(Array(Set(1), Set(1)))
    }
  }

  "Calling applyBCPruning for AllDifferent on domains [0,1] [0,1]" should "return domains [0,1] [0,1]" in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(0, 1), Set(0, 1)))
    val b: Array[Set[Int]] = Array(Set(0, 1), Set(0, 1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling applyBCPruning for AllDifferent on domains [0,1] [0,1] [0,1]" should "return an exception" in {
    assertThrows[NoSolutionException] {
      C.applyBCPruning(Array(Set(0, 1), Set(0, 1), Set(0, 1)))
    }
  }

  "Calling applyBCPruning for AllDifferent on domains [0,1] [1]" should "return domains [0] [1]" in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(0, 1), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling applyBCPruning for AllDifferent on domains [0,1,2] [2,3] [1]" should "return domains [0,1,2] [2,3] [1]" in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(0, 1, 2), Set(2, 3), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0, 1, 2), Set(2, 3), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling applyBCPruning for AllDifferent on domains [0,1,2] [1,2] [2,3] [0]" should "return domains [1,2] [1,2] [3] [0]" in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(0, 1, 2), Set(1, 2), Set(2, 3), Set(0)))
    val b: Array[Set[Int]] = Array(Set(1, 2), Set(1, 2), Set(3), Set(0))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling applyBCPruning for AllDifferent on domains [1,2,3] [0,5] [1,5,6] [1,2] [2,3] [0]" should "return domains [1,2,3] [0,5] [5,6] [1,2] [2,3] [0]" in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(1, 2, 3), Set(0, 5), Set(1, 5, 6), Set(1, 2), Set(2, 3), Set(0)))
    val b: Array[Set[Int]] = Array(Set(1, 2, 3), Set(5), Set(6), Set(1, 2), Set(2, 3), Set(0))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling applyBCPruning for AllDifferent on domains [0,3,4] [1,3], [4], [1]" should "return domains [0] [3] [4] [1] " in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(0, 3, 4), Set(1, 3), Set(4), Set(1)))
    val b: Array[Set[Int]] = Array(Set(0), Set(3), Set(4), Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "Calling applyBCPruning for AllDifferent on domains [0,3,6,8,10] [0,1] [10] [1] [6]" should "return domains [3,6,8] [0,1] [10] [1] [6] " in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(0, 3, 6, 8, 10), Set(0, 1), Set(10), Set(1), Set(6)))
    val b: Array[Set[Int]] = Array(Set(3, 6, 8), Set(0), Set(10), Set(1), Set(6))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "calling applyBCPruning for AllDifferent on domain [1]" should "return [1]" in {
    val a: Array[Set[Int]] = C.applyBCPruning(Array(Set(1)))
    val b: Array[Set[Int]] = Array(Set(1))
    assert((a zip b).forall(x => x._1.equals(x._2)))
  }

  "calling applyBCPruning for false constraint on domain [1,2,3]" should "throw a noSolutionException" in {
    val C = new PruningConstraint{
      override def checker(solution: Array[Int]): Boolean = false
    }
    assertThrows[NoSolutionException] {
      C.applyBCPruning(Array(Set(1, 2, 3)))
    }
  }
}
