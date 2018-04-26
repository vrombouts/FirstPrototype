package unit.constraints

import checker.constraints.{Constraint, Table}
import unit.UnitSpec

class TableTests extends UnitSpec {

  //verification of the checker of Table.
  "checker [1,2,3]" should "be the only value returning true with a table containing [1,2,3]" in {
    val t = new Table(Set(Array(1, 2, 3)))
    assert(t.checker(Array(1, 2, 3)))
    assert(!t.checker(Array(1)))
    assert(!t.checker(Array(2)))
    assert(!t.checker(Array(3)))
    assert(!t.checker(Array(1, 2)))
    assert(!t.checker(Array(1, 3)))
    assert(!t.checker(Array(1, 2, 3, 4)))
    assert(!t.checker(Array(1, 1, 2, 3)))
  }

  "checker" should "return true with an Array contained in the set of the Table" in {
    val t = new Table(Set(Array(1, 2, 3), Array(1, 2), Array(1, 2, 3, 4), Array(1, 1)))
    assert(t.checker(Array(1, 1)))
    assert(t.checker(Array(1, 2)))
    assert(t.checker(Array(1, 2, 3)))
    assert(t.checker(Array(1, 2, 3, 4)))
    assert(!t.checker(Array(1)))
    assert(!t.checker(Array(2)))
    assert(!t.checker(Array(3)))
    assert(!t.checker(Array(1, 3)))
    assert(!t.checker(Array(4, 1, 2, 3)))
  }

  val t = new Table(Set(Array(1, 2, 3), Array(1, 2, 4), Array(1, 3, 4), Array(1, 1, 3)))

  "applyConstraintAC" should "reduce x0,x1 and x2 to {1},{1,2,3},{3,4}" in {
    val v = Array(Set(1, 2, 3, 4), Set(1, 2, 3, 4), Set(1, 2, 3, 4))
    assert(t.applyConstraintAC(v).sameElements(Array(Set(1), Set(1, 2, 3), Set(3, 4))))
  }

  "applyConstraintAC" should "not increase the domain if x1 included in {1,2,3} or x2 in {3,4}" in {
    val v = Array(Set(1), Set(1, 2), Set(3, 4))
    assert(t.applyConstraintAC(v).sameElements(Array(Set(1), Set(1, 2), Set(3, 4))))
    val v2 = Array(Set(1), Set(1, 2, 3), Set(4))
    assert(t.applyConstraintAC(v2).sameElements(Array(Set(1), Set(2, 3), Set(4))))
  }

  "applyConstraintAC" should "reduce the variables correctly while not increasing the domains to match the tables" in {
    val v = Array(Set(1, 2, 3, 4), Set(1, 3, 5), Set(1, 2, 4))
    assert(t.applyConstraintAC(v).sameElements(Array(Set(1), Set(3), Set(4))))
  }

  "Constraint.checkAC" should "run the good number of tests with Table.applyConstraintAC as filtering and table.checker as checker" in {
    val c = new Constraint
    c.gen.setSeed(1)
    c.gen.setNbTests(40)
    c.checkAC(t.applyConstraintAC, t.checker)
    assert(c.stats.getNbExecutedTests == 40)
  }

  "table.checkAC" should "run the good number of tests + its limitCases if applyConstraintAC is used as filtering" in {
    t.gen.setSeed(11)
    t.gen.setNbTests(40)
    t.checkAC(t.applyConstraintAC, null)
    assert(t.stats.getNbExecutedTests == 40 + t.limitCases.length)
  }
}
