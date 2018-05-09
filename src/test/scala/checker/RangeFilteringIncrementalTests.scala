package checker

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}
import org.scalatest.FlatSpec

import scala.util.Random

class RangeFilteringIncrementalTests extends FlatSpec {

  def dummyChecker: Array[Int] => Boolean = _ => true

  val dummyFiltering = new RangeFilteringIncremental(dummyChecker)

  val allDifferentFiltering = new RangeFilteringIncremental(Checkers.allDifferent())

  "setup function" should "correctly perform range filtering" in{
    val dummy:Filter = new ACFiltering(dummyChecker)
    val variables:Array[Set[Int]] = Array(Set(1,2,3), Set(2))
    assert(dummy.filter(variables).sameElements(dummyFiltering.setup(variables)))
    val allDif:Filter= new RangeFiltering(Checkers.allDifferent())
    assert(allDif.filter(variables).sameElements(allDifferentFiltering.setup(variables)))
  }

  "domain storage" should "be empty when beginning" in {
    val a = Array(Set(1, 2), Set(1, 2))
    val b: Array[Set[Int]] = Array()
    assert(dummyFiltering.pop(a).sameElements(a))
    assert(dummyFiltering.pop(b).sameElements(b))
  }

  "domain storage when pushed then popped" should "return the variables pushed" in {
    val a = Array(Set(1, 2), Set(1, 2))
    val b: Array[Set[Int]] = Array()
    assert(dummyFiltering.push(a).sameElements(a))
    assert(dummyFiltering.pop(b).sameElements(a))
    assert(dummyFiltering.push(b).sameElements(b))
    assert(dummyFiltering.pop(a).sameElements(b))
  }

  "pushes" should "be stacked waiting for their pop (that could never come)" in {
    val a = Array(Set(1, 2), Set(1, 2))
    val c = Array(Set(1, 2, 3), Set(2, 3, 4))
    val b: Array[Set[Int]] = Array()
    assert(dummyFiltering.push(a).sameElements(a))
    assert(dummyFiltering.push(b).sameElements(b))
    assert(dummyFiltering.push(c).sameElements(c))
    assert(dummyFiltering.pop(a).sameElements(c))
    assert(dummyFiltering.pop(a).sameElements(b))
  }

  "branchAndFilter receiving a Push" should "push the domains in Push" in {
    val pushBranch: BranchOp = new Push(Array(Set(1, 2), Set(1, 2)))
    assert(dummyFiltering.branchAndFilter(pushBranch).sameElements(pushBranch.domains))
  }

  "branchAndFilter receiving a Pop" should "return the domains of previous Push" in {
    val popBranch: BranchOp = new Pop(Array(Set(1)))
    assert(dummyFiltering.branchAndFilter(popBranch).sameElements(Array(Set(1, 2), Set(1, 2))))
  }

  "branchAndFilter receiving a RestrictDomain" should "return the restricted domain" in {
    val restrict = new RestrictDomain(Array(Set(1, 2), Set(1, 2)), new Random(1))
    //all combinations not tested since RestrictDomain class is
    // already tested in RestrictDomainTests.scala
    restrict.index = 0
    restrict.constant = 1
    restrict.op = "="
    assert(dummyFiltering.branchAndFilter(restrict).sameElements(Array(Set(1), Set(1, 2))))
  }


  "branchAndFilter receiving a RestrictDomain" should "return the restrict domain filtered by branchAndFilter taking variables in arguments" in {
    val restrict = new RestrictDomain(Array(Set(1, 2), Set(1, 3), Set(1, 2)), new Random(100))
    restrict.index = 0
    restrict.constant = 1
    restrict.op = "="
    assert(allDifferentFiltering.branchAndFilter(restrict).sameElements(Array(Set(1), Set(3), Set(2))))
  }


  "branchAndFilter " should "have the push and pop work correctly even when there is another call in between" in {
    val pushBranch: BranchOp = new Push(Array(Set(1, 2), Set(1, 2)))
    val popBranch: BranchOp = new Pop(Array(Set(), Set()))
    val restrict = new RestrictDomain(Array(Set(1, 2), Set(1, 3), Set(1, 2)), new Random(100))
    restrict.index = 0
    restrict.constant = 1
    restrict.op = "="
    allDifferentFiltering.branchAndFilter(pushBranch)
    allDifferentFiltering.branchAndFilter(restrict)
    assert(allDifferentFiltering.branchAndFilter(popBranch).sameElements(pushBranch.domains))
  }

  "branchAndFilter with another operation than a RestrictDomain,Push or Pop" should "just return the domains of the BranchOp" in {
    val branchOp = new BranchOp(Array())
    assert(allDifferentFiltering.branchAndFilter(branchOp).isEmpty)
  }

}
