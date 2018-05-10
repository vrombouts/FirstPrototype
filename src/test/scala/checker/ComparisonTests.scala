package checker

import org.scalatest.FlatSpec

class ComparisonTests extends FlatSpec {

  "comparisonCheck" should "return true if v(1) is the same as v(2)" in {
    var v = Array(
      Array(Set(1)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1), Set(2)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1)),
      Array(Set(1), Set(2)),
      Array(Set(1), Set(2))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(),
      Array(),
      Array()
    )
    assert(CPChecker.comparisonCheck(v))
  }
  //only testing if all elements of v have the same length (pre-condition)
  "comparisonCheck" should "return false if v(1) is included in v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set()),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3)),
      Array(Set(1, 2), Set(1, 2, 3))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(2) is included in v(1)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set(1))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set())
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(1) and v(2) included in v(0) and v(1)!=v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2, 3)),
      Array(Set(2, 1)),
      Array(Set(2, 3))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(1) is null" in {
    val v = Array(
      Array(Set(1, 2)),
      null,
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return false if v(1)'s length is different than v(2)'s length" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1), Set(1)),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2)),
      Array(),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonCheck(v))
  }

  "comparisonCheck" should "return true if v(1) and v(2) both possess an empty set" in {
    var v: Array[Array[Set[Int]]] = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(1), Set()),
      Array(Set(1), Set())
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(2), Set()),
      Array(Set(1), Set())
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(2), Set()),
      Array(Set(), Set(2))
    )
    assert(CPChecker.comparisonCheck(v))
    v = Array(
      Array(Set(1, 2), Set(2)),
      Array(Set(), Set()),
      Array(Set(1), Set())
    )
    assert(CPChecker.comparisonCheck(v))
  }

  "comparisonStronger" should "return true if v(1) is the same as v(2)" in {
    var v = Array(
      Array(Set(1)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1), Set(2)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1)),
      Array(Set(1), Set(2)),
      Array(Set(1), Set(2))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(),
      Array(),
      Array()
    )
    assert(CPChecker.comparisonStronger(v))
  }
  //only testing if all elements of v have the same length (pre-condition)
  "comparisonStronger" should "return false if v(1) is included in v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set()),
      Array(Set(1, 2))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3)),
      Array(Set(1, 2), Set(1, 2, 3))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2))
    )
    assert(!CPChecker.comparisonStronger(v))
  }

  "comparisonStronger" should "return true if v(2) is included in v(1)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set(1))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2)),
      Array(Set(1, 2)),
      Array(Set())
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1, 2), Set(1, 2, 3)),
      Array(Set(1), Set(1, 3))
    )
    assert(CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2, 3), Set(1, 2)),
      Array(Set(1, 2), Set(1, 2), Set(1, 2))
    )
    assert(CPChecker.comparisonStronger(v))
  }

  "comparisonStronger" should "return false if v(1) and v(2) included in v(0) and v(1)!=v(2)" in {
    var v = Array(
      Array(Set(1, 2)),
      Array(Set(1)),
      Array(Set(2))
    )
    assert(!CPChecker.comparisonStronger(v))
    v = Array(
      Array(Set(1, 2, 3)),
      Array(Set(2, 1)),
      Array(Set(2, 3))
    )
    assert(!CPChecker.comparisonStronger(v))
  }

}
