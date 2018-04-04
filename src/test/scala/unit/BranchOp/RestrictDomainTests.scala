package unit.BranchOp

import checker.constraints.incremental.RestrictDomain
import unit.UnitSpec

import scala.util.Random

class RestrictDomainTests extends UnitSpec{
  val set1= Set(1,2)
  val set2 = Set(2)
  val set3=Set(3,4)
  val domains = Array(set1,set2,set3)

  "RestrictDomain" should "set randomly its own restriction (x_index op constant) intelligently" in {
    val rd = new RestrictDomain(domains, new Random(1000))
    assert(rd.index==0 || rd.index==2)
  }

}
