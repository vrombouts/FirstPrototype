package checker.incremental


import checker.constraints.incremental.BranchOp
import org.scalatest.FlatSpec

class BranchOpTests extends FlatSpec {
  val set1= Set(1)
  val set2 = Set(2)
  val set3=Set(3)
  val domains = Array(set1,set2,set3)

  "constructor" should "not modify the given domains" in {
    val bop = new BranchOp(domains)
    assert(bop.domains.sameElements(domains))
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
