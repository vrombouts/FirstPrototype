package unit.branchop

import checker.constraints.incremental.Push
import unit.UnitSpec

class PushTests extends UnitSpec {
  val set1 = Set(1)
  val set2 = Set(2)
  val set3 = Set(3)
  val domains = Array(set1, set2, set3)

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
  "toString" should " return Push" in {
    val push = new Push(domains)
    assert(push.toString.equals("Push"))
  }
}
