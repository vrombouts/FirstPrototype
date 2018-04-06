package unit

import checker.Op
import scala.util.Random

class OpTests extends UnitSpec{
  "opposite for an existing operation" should "return the opposite operation" in {
    assert(Op.opposite(">")=="<=")
    assert(Op.opposite(">=")=="<")
    assert(Op.opposite("<")==">=")
    assert(Op.opposite("<=")==">")
    assert(Op.opposite("=")=="!=")
    assert(Op.opposite("!=")=="=")
  }

  "opposite for a non existing operation" should "return 'no such op'" in {
    assert(Op.opposite("pp")=="no such op")
  }

  "respectOp for 1 op 2" should "return true for <,<=,!=, false otherwise" in {
    assert(Op.respectOp("<",1,2))
    assert(Op.respectOp("<=",1,2))
    assert(Op.respectOp("!=",1,2))
    assert(!Op.respectOp(">",1,2))
    assert(!Op.respectOp(">=",1,2))
    assert(!Op.respectOp("=",1,2))
  }

  "respectOp for 0 op 0" should "return true for <=,=,>= false otherwise" in {
    assert(Op.respectOp("<=",0,0))
    assert(Op.respectOp("=",0,0))
    assert(Op.respectOp(">=",0,0))
    assert(!Op.respectOp(">",0,0))
    assert(!Op.respectOp("<",0,0))
    assert(!Op.respectOp("!=",0,0))
  }

  "respectOp for 1 op 0" should "return true for >,>=,!= false otherwise" in {
    assert(Op.respectOp(">",1,0))
    assert(Op.respectOp(">=",1,0))
    assert(Op.respectOp("!=",1,0))
    assert(!Op.respectOp("=",1,0))
    assert(!Op.respectOp("<",1,0))
    assert(!Op.respectOp("<=",1,0))
  }

  "respectOp for an undefined op" should "return as if op== '='" in {
    assert(Op.respectOp("_",2 ,2))
    assert(Op.respectOp("_",-1 ,-1))
    assert(!Op.respectOp("_",1 ,2))
  }

  "randomOp" should "return an existing operation randomly" in {
    for(_ <- 0 until 50)
      assert(Op.opposite(Op.randomOp())!= "no such op")
  }

  "randomOp with a random object with a set seed" should "always return the same op" in {
    for(i <- 0 until 50)
      assert(Op.randomOp(new Random(i))==Op.randomOp(new Random(i)))
  }

  "condition for '='" should "return false if constant is '>=' smin and '<=' smax" in {
    assert(!Op.condition("=",0,0,0))
    assert(!Op.condition("=",1,3,2))
    assert(!Op.condition("=",-1,0,-1))
    assert(!Op.condition("=",0,1,1))
    assert(Op.condition("=",3,1,2))
    assert(Op.condition("=",0,0,-1))
    assert(Op.condition("=",0,0,1))
  }

  "condition for !=" should "return true only if smin==smax==constant" in {
    assert(Op.condition("!=",0,0,0))
    assert(!Op.condition("!=",1,0,0))
    assert(!Op.condition("!=",0,1,0))
    assert(!Op.condition("!=",0,0,1))
    assert(!Op.condition("!=",1,3,2))
  }

  "condition for <" should "return true only if smin>=constant" in {
    assert(Op.condition("<",0,0,0))
    assert(Op.condition("<",1,0,0))
    assert(Op.condition("<",0,1,0))
    assert(!Op.condition("<",0,0,1))
    assert(Op.condition("<",4,3,2))
  }

  "condition for <=" should "return true only if smin>constant" in {
    assert(!Op.condition("<=",0,0,0))
    assert(Op.condition("<=",1,0,0))
    assert(!Op.condition("<=",0,1,0))
    assert(!Op.condition("<=",0,0,1))
    assert(Op.condition("<=",4,3,2))
  }

  "condition for >" should "return true only if smax<=constant" in {
    assert(Op.condition(">",0,0,0))
    assert(Op.condition(">",1,0,0))
    assert(!Op.condition(">",0,1,0))
    assert(Op.condition(">",0,0,1))
    assert(!Op.condition(">",4,3,2))
    assert(Op.condition(">",0,2,3))
    assert(Op.condition(">",0,2,2))
  }

  "condition for >=" should "return true only if smax<constant" in {
    assert(!Op.condition(">=",0,0,0))
    assert(!Op.condition(">=",1,0,0))
    assert(!Op.condition(">=",0,1,0))
    assert(Op.condition(">=",0,0,1))
    assert(!Op.condition(">=",4,3,2))
    assert(Op.condition(">=",0,2,3))
    assert(!Op.condition(">=",0,2,2))
  }

  "condition for an unknown op" should "not throw an exception" in {
    assert({Op.condition("_",0,0,0);true})
  }
}
