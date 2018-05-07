package checker.incremental

import org.scalatest.FlatSpec

class PopTests extends FlatSpec {
  val set1 = Set(1)
  val set2 = Set(2)
  val set3 = Set(3)
  val domains = Array(set1, set2, set3)

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
  "toString" should " return Pop" in {
    val pop = new Pop(domains)
    assert(pop.toString.equals("Pop"))
  }
}
