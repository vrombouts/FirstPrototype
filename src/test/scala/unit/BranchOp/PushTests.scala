package unit.BranchOp

import java.util

import checker.constraints.incremental.Push
import unit.UnitSpec

class PushTests extends UnitSpec{
  val set1: util.Set[Integer] = new util.HashSet()
  set1.add(1)
  val set2: util.Set[Integer] = new util.HashSet()
  set2.add(2)
  val set3: util.Set[Integer] = new util.HashSet()
  set3.add(3)
  val domains = Array(set1,set2,set3)

  "constructor" should "not modify the given domains" in {
    val push = new Push(domains)
    assert(push.domains.sameElements(domains))
  }

  "clone" should "return a branchOp were modifying its domains does not modify the initial BranchOp" in {
    val push = new Push(domains)
    val pushClone = push.clone()
    pushClone.domains(1) = null
    assert(!push.domains.sameElements(pushClone.domains))
  }
  "toString" should " return that there is no more branching" in {
    val push = new Push(domains)
    assert(push.toString.equals("Push\n"))
  }
}
