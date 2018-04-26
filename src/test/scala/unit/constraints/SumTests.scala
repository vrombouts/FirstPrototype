package unit.constraints

import checker.Op
import checker.constraints.{BasicConstraint, Sum}
import unit.UnitSpec

class SumTests extends UnitSpec {
  val eq = new Sum(0)
  val dif = new Sum(Op.different, 0)
  val lt = new Sum(Op.lesserThan, 0)
  val le = new Sum(Op.lesserThanOrEqual, 0)
  val gt = new Sum(Op.greaterThan, 0)
  val ge = new Sum(Op.greaterThanOrEqual, 0)

  //verification checker
  "eq.checker " should "check if the sum is equal to 0" in {
    assert(eq.checker(Array(0, 0, 0, 0)))
    assert(eq.checker(Array(-1, 0, 0, 1)))
    assert(eq.checker(Array(-1, 2, -3, 2)))
    assert(eq.checker(Array(0)))
    assert(!eq.checker(Array(1)))
    assert(!eq.checker(Array(-1)))
    assert(!eq.checker(Array(-1, 2)))
    assert(!eq.checker(Array(-1, 2, -3)))
  }

  "dif.checker " should "check if the sum is not equal to 0" in {
    assert(!dif.checker(Array(0, 0, 0, 0)))
    assert(!dif.checker(Array(-1, 0, 0, 1)))
    assert(!dif.checker(Array(-1, 2, -3, 2)))
    assert(!dif.checker(Array(0)))
    assert(dif.checker(Array(1)))
    assert(dif.checker(Array(-1)))
    assert(dif.checker(Array(-1, 2)))
    assert(dif.checker(Array(-1, 2, -3)))
  }

  "lt.checker " should "check if the sum is less than 0" in {
    assert(!lt.checker(Array(0)))
    assert(!lt.checker(Array(1)))
    assert(!lt.checker(Array(-1, 2)))
    assert(!lt.checker(Array(0, 0, 0, 0)))
    assert(lt.checker(Array(-1)))
    assert(lt.checker(Array(-1, 2, -3)))
    assert(lt.checker(Array(-1, 0, 0, 0)))
    assert(lt.checker(Array(-1, 2, -4, 2)))
  }

  "le.checker " should "check if the sum is less than or equal to 0" in {
    assert(le.checker(Array(0)))
    assert(!le.checker(Array(1)))
    assert(!le.checker(Array(-1, 2)))
    assert(le.checker(Array(-1, 0, 0, 1)))
    assert(le.checker(Array(-1)))
    assert(le.checker(Array(-1, 2, -3)))
    assert(le.checker(Array(-1, 0, 0, 0)))
    assert(le.checker(Array(-1, 2, -4, 2)))
  }

  "gt.checker " should "check if the sum is greater than 0" in {
    assert(!gt.checker(Array(0)))
    assert(gt.checker(Array(1)))
    assert(gt.checker(Array(-1, 2)))
    assert(!gt.checker(Array(0, 0, 0, 0)))
    assert(!gt.checker(Array(-1)))
    assert(!gt.checker(Array(-1, 2, -3)))
    assert(!gt.checker(Array(-1, 0, 0, 0)))
    assert(!gt.checker(Array(-1, 2, -4, 2)))
  }

  "ge.checker " should "check if the sum is greater than or equal to 0" in {
    assert(ge.checker(Array(0)))
    assert(ge.checker(Array(1)))
    assert(ge.checker(Array(-1, 2)))
    assert(ge.checker(Array(-1, 0, 1, 0)))
    assert(!ge.checker(Array(-1)))
    assert(!ge.checker(Array(-1, 2, -3)))
    assert(!ge.checker(Array(-1, 0, 0, 0)))
    assert(!ge.checker(Array(-1, 2, -4, 2)))
  }

  "a Sum with an unsupported operation" should "have its checker always returning true" in {
    val unsupported = new Sum("unsupported operation", 0)
    assert(unsupported.checker(Array(0)))
    assert(unsupported.checker(Array(1)))
    assert(unsupported.checker(Array(-1, 2)))
    assert(unsupported.checker(Array(-1, 0, 1, 0)))
    assert(unsupported.checker(Array(-1)))
    assert(unsupported.checker(Array(-1, 2, -3)))
    assert(unsupported.checker(Array(-1, 0, 0, 0)))
    assert(unsupported.checker(Array(-1, 2, -4, 2)))
  }

  "eq.checkAC with BCBasic as filtering on 40 tests" should "run 40+limitCases tests" in {
    val sum = eq
    val b = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = {
        sum.checker(solution)
      }
    }
    sum.setGen(6)
    sum.gen.setSeed(1111)
    sum.gen.setNbTests(40)
    sum.checkAC(b.applyBCWithoutPruning, null)
    assert(sum.stats.getNbExecutedTests == 40 + sum.limitCases.length)
  }

  "dif.checkAC with BCBasic as filtering on 40 tests" should "run 40+limitCases tests" in {
    val sum = dif
    val b = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = {
        sum.checker(solution)
      }
    }
    sum.setGen(6)
    sum.gen.setSeed(1111)
    sum.gen.setNbTests(40)
    sum.checkAC(b.applyBCWithoutPruning, null)
    assert(sum.stats.getNbExecutedTests == 40 + sum.limitCases.length)
  }

  "lt.checkAC with BCBasic as filtering on 40 tests" should "run 40+limitCases tests" in {
    val sum = lt
    val b = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = {
        sum.checker(solution)
      }
    }
    sum.setGen(6)
    sum.gen.setSeed(1111)
    sum.gen.setNbTests(40)
    sum.checkAC(b.applyBCWithoutPruning, null)
    assert(sum.stats.getNbExecutedTests == 40 + sum.limitCases.length)
  }

  "le.checkAC with BCBasic as filtering on 40 tests" should "run 40+limitCases tests" in {
    val sum = le
    val b = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = {
        sum.checker(solution)
      }
    }
    sum.setGen(6)
    sum.gen.setSeed(1111)
    sum.gen.setNbTests(40)
    sum.checkAC(b.applyBCWithoutPruning, null)
    assert(sum.stats.getNbExecutedTests == 40 + sum.limitCases.length)
  }

  "gt.checkAC with BCBasic as filtering on 40 tests" should "run 40+limitCases tests" in {
    val sum = gt
    val b = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = {
        sum.checker(solution)
      }
    }
    sum.setGen(6)
    sum.gen.setSeed(1111)
    sum.gen.setNbTests(40)
    sum.checkAC(b.applyBCWithoutPruning, null)
    assert(sum.stats.getNbExecutedTests == 40 + sum.limitCases.length)
  }

  "ge.checkAC with BCBasic as filtering on 40 tests" should "run 40+limitCases tests" in {
    val sum = ge
    val b = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = {
        sum.checker(solution)
      }
    }
    sum.setGen(6)
    sum.gen.setSeed(1111)
    sum.gen.setNbTests(40)
    sum.checkAC(b.applyBCWithoutPruning, null)
    assert(sum.stats.getNbExecutedTests == 40 + sum.limitCases.length)
  }

}
