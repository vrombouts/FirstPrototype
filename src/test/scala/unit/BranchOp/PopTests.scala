package unit.BranchOp

import java.util

import checker.constraints.incremental.Pop
import unit.UnitSpec

class PopTests extends UnitSpec{
  val set1: util.Set[Integer] = new util.HashSet()
  set1.add(1)
  val set2: util.Set[Integer] = new util.HashSet()
  set2.add(2)
  val set3: util.Set[Integer] = new util.HashSet()
  set3.add(3)
  val domains = Array(set1,set2,set3)

  "constructor" should "not modify the given domains" in {
    val pop = new Pop(domains)
    assert(pop.domains.sameElements(domains))
  }

  "clone" should "return a branchOp were modifying its domains does not modify the initial BranchOp" in {
    val pop = new Pop(domains)
    val popClone = pop.clone()
    popClone.domains(1) = null
    assert(!pop.domains.sameElements(popClone.domains))
  }
  "toString" should " return that there is no more branching" in {
    val pop = new Pop(domains)
    assert(pop.toString.equals("Pop\n"))
  }
}
