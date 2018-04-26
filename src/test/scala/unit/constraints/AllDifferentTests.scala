package unit.constraints

import checker.constraints.AllDifferent
import unit.UnitSpec

class AllDifferentTests extends UnitSpec {
  val allDif = new AllDifferent

  "allDif propagation values " should "be correctly set" in{
    assert(allDif.AC == 1)
    assert(allDif.BC == 2)
    assert(allDif.notSpecified == 0)
  }

  "[1,1]" should "return false in allDifferent" in {
    assert(!allDif.checker(Array(1, 1)))
  }

  "[1,0]" should "return true in allDifferent" in {
    assert(allDif.checker(Array(1, 0)))
  }

  "[1,0,1]" should "return false in allDifferent" in {
    assert(!allDif.checker(Array(1, 0, 1)))
  }

  "[0,1,2,3,4,5,6,7,8,9,10,11,12,0]" should "return false in allDifferent" in {
    assert(!allDif.checker(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0)))
  }

  "[0,1,2,3,4,5,6,7,8,9,10,11,12]" should "return true in allDifferent" in {
    assert(allDif.checker(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
  }

  "[-1,-1]" should "return false in allDifferent" in {
    assert(!allDif.checker(Array(-1, -1)))
  }

  "[-1,1]" should "return true in allDifferent" in {
    assert(allDif.checker(Array(-1, 1)))
  }

  "[-1,0]" should "return true in allDifferent" in {
    assert(allDif.checker(Array(-1, 0)))
  }

  //testing with limit Cases
  "checkAC" should " perform the limitCases of allDifferent AC constraint" in {
    val limitCases = allDif.limitCases
    //limitCases should exists
    assert(limitCases.length != 0)
    allDif.gen.setNbTests(1)
    val ar = Array(0)
    allDif.checkAC(x => {
      ar(0) = ar(0) + 1
      allDif.applyACPruning(x)
    }, null)
    assert(ar(0) == limitCases.length + 1)
  }
}
