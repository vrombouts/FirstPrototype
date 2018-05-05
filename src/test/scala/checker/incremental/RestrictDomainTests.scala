package checker.incremental

import checker.Op
import org.scalatest.FlatSpec

import scala.util.Random

class RestrictDomainTests extends FlatSpec {

  "RestrictDomain" should "set randomly its own restriction (x_index op constant) intelligently" in {
    val set1 = Set(1, 2)
    val set2 = Set(2)
    val set3 = Set(3, 4)
    val domains = Array(set1, set2, set3)
    val rd = new RestrictDomain(domains, new Random(1000))
    assert(rd.index == 0 || rd.index == 2)
    if (rd.index == 0) assert(rd.constant == 1 || rd.constant == 2)
    if (rd.index == 2) assert(rd.constant == 3 || rd.constant == 4)
    if (rd.op.equals("<")) assert(rd.constant == 4)
    if (rd.op.equals(">")) assert(rd.constant == 3)
    assert(rd.op.equals(Op.equal) || rd.op.equals(Op.different) || rd.op.equals(Op.greaterThan) ||
      rd.op.equals(Op.lesserThanOrEqual) || rd.op.equals(Op.greaterThanOrEqual) || rd.op.equals(Op.lesserThan))
  }

  "a clone of a RestrictDomain" should "have the same domain, index, op and constant" in {
    val set1 = Set(1, 2)
    val set2 = Set(2)
    val set3 = Set(3, 4)
    val domains = Array(set1, set2, set3)
    val rd = new RestrictDomain(domains, new Random(1000))
    val rdCloned: RestrictDomain = rd.clone.asInstanceOf[RestrictDomain]
    assert((rd.domains zip rdCloned.domains).forall(x => x._1.equals(x._2)))
    assert(rd.index == rdCloned.index)
    assert(rd.op.equals(rdCloned.op))
    assert(rd.constant.equals(rdCloned.constant))
  }

  "a change to the clone of a RestrictDomain" should "not modify the initial RestrictDomain" in {
    val set1 = Set(1, 2)
    val set2 = Set(2)
    val set3 = Set(3, 4)
    val domains = Array(set1, set2, set3)
    val rd = new RestrictDomain(domains, new Random(1000))
    val ind: Int = rd.index
    val op: String = rd.op
    val cst: Int = rd.constant
    val rdCloned: RestrictDomain = rd.clone.asInstanceOf[RestrictDomain]
    rdCloned.index += 1
    rdCloned.domains(0) += 4
    rdCloned.constant += 1
    if (rdCloned.op.equals(Op.equal)) rdCloned.op = Op.different
    else rdCloned.op = Op.equal
    assert((rd.domains zip domains).forall(x => x._1.equals(x._2)))
    assert(rd.index == ind)
    assert(rd.op.equals(op))
    assert(rd.constant == cst)
  }

  "toString of RestrictDomain" should "contain the x_index, the operation and the constant in this order" in {
    val set1 = Set(1, 2)
    val set2 = Set(2)
    val set3 = Set(3, 4)
    val domains = Array(set1, set2, set3)
    val rd = new RestrictDomain(domains, new Random(1000))
    val res: String = rd.toString
    assert(res.contains("x_" + rd.index + rd.op + rd.constant))
  }

  "applyRestriction of RestrictDomain" should "reduce the domain accordingly" in {
    val set1 = Set(1, 2)
    val set2 = Set(2)
    val set3 = Set(3, 4)
    val domains = Array(set1, set2, set3)
    val rd = new RestrictDomain(domains, new Random(1000))
    rd.index = 2
    rd.op = Op.equal
    rd.constant = 4
    assert(rd.applyRestriction(0).equals(set1))
    assert(rd.applyRestriction(1).equals(set2))
    assert(rd.applyRestriction(2).equals(Set(4)))
  }
}
