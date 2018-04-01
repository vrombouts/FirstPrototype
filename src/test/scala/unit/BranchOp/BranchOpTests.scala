package unit.BranchOp

import java.util

import checker.constraints.incremental.{BranchOp, BranchOpScala}
import unit.UnitSpec
import java.util.Set
class BranchOpTests extends UnitSpec{
  val set1: util.Set[Integer] = new util.HashSet()
  set1.add(1)
  val set2: util.Set[Integer] = new util.HashSet()
  set2.add(2)
  val set3: util.Set[Integer] = new util.HashSet()
  set3.add(3)
  val domains = Array(set1,set2,set3)

  "constructor" should "not modify the given domains" in {
    val bop = new BranchOp(domains)
    assert(bop.domains.sameElements(domains))
    val bop2 = new BranchOpScala(domains)
    assert(bop2.domains.sameElements(domains))
  }

  "clone" should "return a branchOp were modifying its domains does not modify the initial BranchOp" in {
    val bop = new BranchOp(domains)
    val bopClone = bop.clone()
    bopClone.domains(1) = null
    assert(!bop.domains.sameElements(bopClone.domains))
  }
  "toString" should " return that there is no more branching" in {
    val bop = new BranchOp(domains)
    assert(bop.toString.equals("No more branching possible"))
  }
}
