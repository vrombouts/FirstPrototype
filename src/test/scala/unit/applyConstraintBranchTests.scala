package unit

import checker.constraints.Checker
import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}

import scala.util.Random

class applyConstraintBranchTests extends UnitSpec {

  private[this] object dummyCheck extends Checker{
    def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = variables
  }

  private[this] object SpecialCheck extends Checker{
    def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
      variables(2) = Set(5)
      variables
    }
  }

  "domain storage" should "be empty when beginning" in {
    val a = Array(Set(1,2),Set(1,2))
    val b :Array[Set[Int]]= Array()
    assert(dummyCheck.pop(a).sameElements(a))
    assert(dummyCheck.pop(b).sameElements(b))
  }

  "domain storage when pushed then popped" should "return the variables pushed" in {
    val a = Array(Set(1,2),Set(1,2))
    val b :Array[Set[Int]]= Array()
    assert(dummyCheck.push(a).sameElements(a))
    assert(dummyCheck.pop(b).sameElements(a))
    assert(dummyCheck.push(b).sameElements(b))
    assert(dummyCheck.pop(a).sameElements(b))
  }

  "pushes" should "be stacked waiting for their pop (that could never come)" in {
    val a = Array(Set(1,2),Set(1,2))
    val c = Array(Set(1,2,3),Set(2,3,4))
    val b :Array[Set[Int]]= Array()
    assert(dummyCheck.push(a).sameElements(a))
    assert(dummyCheck.push(b).sameElements(b))
    assert(dummyCheck.push(c).sameElements(c))
    assert(dummyCheck.pop(a).sameElements(c))
    assert(dummyCheck.pop(a).sameElements(b))
  }

  "applyConstraint receiving a Push" should "push the domains in Push" in {
    val pushBranch: BranchOp = new Push(Array(Set(1,2),Set(1,2)))
    assert(dummyCheck.applyConstraint(pushBranch).sameElements(pushBranch.domains))
  }

  "applyConstraint receiving a Pop" should "return the domains of previous Push" in {
    val popBranch:BranchOp = new Pop(Array(Set(1)))
    assert(dummyCheck.applyConstraint(popBranch).sameElements(Array(Set(1,2),Set(1,2))))
  }

  "applyConstraint receiving a RestrictDomain" should "return the restricted domain" in {
    val restrict = new RestrictDomain(Array(Set(1,2),Set(1,2)),new Random(1))
    //all combinations not tested since RestrictDomain class is
    // already tested in RestrictDomainTests.scala
    restrict.index = 0
    restrict.constant = 1
    restrict.op = "="
    assert(dummyCheck.applyConstraint(restrict).sameElements(Array(Set(1),Set(1,2))))
  }



  "applyConstraint receiving a RestrictDomain" should "return the restrict domain filtered by applyConstraint taking vriables in arguments" in {
    val restrict = new RestrictDomain(Array(Set(1,2),Set(1,3),Set(1,2)),new Random(100))
    restrict.index = 0
    restrict.constant = 1
    restrict.op = "="
    assert(SpecialCheck.applyConstraint(restrict).sameElements(Array(Set(1),Set(1,3),Set(5))))
  }


  "applyConstraint " should "have the push and pop unperturbed by another call in between" in {

    val pushBranch: BranchOp = new Push(Array(Set(1,2),Set(1,2)))
    val popBranch: BranchOp = new Pop(Array(Set(),Set()))
    val restrict = new RestrictDomain(Array(Set(1,2),Set(1,3),Set(1,2)),new Random(100))
    restrict.index = 0
    restrict.constant = 1
    restrict.op = "="
    SpecialCheck.applyConstraint(pushBranch)
    SpecialCheck.applyConstraint(restrict)
    assert(SpecialCheck.applyConstraint(popBranch).sameElements(pushBranch.domains))
  }

  "applyConstraint with not a RestrictDomain,Push or Pop" should "just return the domains of the BranchOp" in {
    val branchOp = new BranchOp(Array())
    assert(SpecialCheck.applyConstraint(branchOp).isEmpty)
  }

}
