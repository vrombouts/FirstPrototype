package unit.constraints

import checker.constraints.{BasicConstraint, Element}
import unit.UnitSpec
/*
 * x_i=v
 * [x_0,x_1,...x_n,i,v]
 */
class ElementTests extends UnitSpec{
  val e = new Element

  //verification checker
  "check x0=5 ([5 0 5])" should "be true and all other vale of x_0 should be false" in{
    assert(e.checker(Array(5,0,5)))
    assert(!e.checker(Array(0,0,5)))
    assert(!e.checker(Array(6,0,5)))
    assert(!e.checker(Array(4,0,5)))
    assert(!e.checker(Array(-5,0,5)))
  }

  "checker" should "notice correctly the value of i" in {
    assert(e.checker(Array(5,1,2,0,5)))
    assert(e.checker(Array(5,1,2,1,1)))
    assert(e.checker(Array(5,1,2,2,2)))
    assert(!e.checker(Array(5,1,2,0,1)))
    assert(!e.checker(Array(5,1,2,1,2)))
    assert(!e.checker(Array(5,1,2,2,5)))
  }

  "checker" should "return false if the solution has less than 3 element" in {
    assert(!e.checker(Array(0,1)))
    assert(!e.checker(Array(0,0)))
    assert(!e.checker(Array(1,0)))
  }

  "checker" should "return false if i is bigger than solution.length-3" in {
    assert(!e.checker(Array(5,1,2,0)))
    assert(!e.checker(Array(5,1,2,1)))
    assert(!e.checker(Array(5,1,2,2)))
    assert(!e.checker(Array(5,1,2,3,3)))
    assert(!e.checker(Array(5,1,2,4,1)))
  }

  "checker" should "return false if i<0" in {
    assert(!e.checker(Array(5,1,2,-1,1)))
  }

  "checkAC for 40 tests and applyACWithoutFiltering of BasicConstraint with the Element checker" should "run 40 tests + its limitCases" in {
    val c = new BasicConstraint {
      override def checker(solution: Array[Int]): Boolean = { e.checker(solution)}
    }
    e.setGen(7)
    e.gen.setNbTests(40)
    e.gen.setSeed(111)
    e.checkAC(c.applyACWithoutPruning,null)
    assert(e.stats.getNbExecutedTests==40+e.limitCases().length)
  }
}
