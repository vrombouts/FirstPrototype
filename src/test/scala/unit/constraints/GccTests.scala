package unit.constraints

import checker.constraints.{BasicConstraint, Gcc}
import unit.UnitSpec

class GccTests extends UnitSpec {
  val gcc = new Gcc(Array(1,2,3))
  //checking the checker of Gcc
  "[1,2,3,1,1,1]" should "return true for Gcc" in {
    assert(gcc.checker(Array(1,2,3,1,1,1)))
  }

  "[-1,2,3,0,1,1]" should "return true for Gcc" in {
    assert(gcc.checker(Array(-1,2,3,0,1,1)))
  }

  "[3,2,1,1,1,1]" should "return true for Gcc" in {
    assert(gcc.checker(Array(3,2,1,1,1,1)))
  }

  "[3,3,3,1,1,1]" should "return false for Gcc" in {
    assert(!gcc.checker(Array(3,3,3,1,1,1)))
  }

  "[3,3,3,0,0,3]" should "return true for Gcc" in {
    assert(gcc.checker(Array(3,3,3,0,0,3)))
  }
  "[1,1,1,2,3,2,1,2,2,1,3,1,3,2,1,3,7,5,4]" should "return true for Gcc" in {
    assert(gcc.checker(Array(1,1,1,2,3,2,1,2,2,1,3,1,3,2,1,3,7,5,4)))
  }

  "[1,2,3,0,0,0]" should "return false for Gcc" in {
    assert(!gcc.checker(Array(1,2,3,0,0,0)))
  }
  "[0,5,4,0,0,0]" should "return true for Gcc" in {
    assert(gcc.checker(Array(0,5,4,0,0,0)))
  }

  "[0,0,3,0,0,0]" should "return false for Gcc" in {
    assert(!gcc.checker(Array(0,0,3,0,0,0)))
  }

  "[0,0]" should "return false for Gcc" in {
    assert(!gcc.checker(Array(0,0)))
  }

  "[0,0,0]" should "return true for Gcc" in {
    assert(gcc.checker(Array(0,0,0)))
  }

  //testing filtering of GCC
  "gcc special AC filtering" should "behave like a BasicConstraint with the checker of Gcc" in {
    val c = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = {
        gcc.checker(solution)
      }
    }
    gcc.gen.setNbTests(40)
    gcc.gen.setSeed(1)
    //checkAC run 40 tests if no fails + the limitCases of Gcc
    gcc.checkAC(c.applyAC, null)
    assert(gcc.stats.getNbExecutedTests==40 + gcc.limitCases.length)
  }



}
