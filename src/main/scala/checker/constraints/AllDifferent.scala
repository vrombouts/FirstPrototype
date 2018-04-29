package checker.constraints

class AllDifferent extends ConstraintPruning {
  gen.setRangeForAll((-5, 5))
  gen.setDensityForAll(0.3)
  gen.setNVar(7)

  override def checker(solution: Array[Int]): Boolean = solution.toSet.size == solution.length

  override def limitCases: Array[Array[Set[Int]]] = {
    Array(
      Array(Set(0, 1, 2), Set(0)),
      Array(Set(0, 1), Set(0, 1), Set(0, 1, 2)),
      Array(Set(0), Set(1), Set(2)),
      Array(Set(0, 1), Set(1, 2), Set(2, 3), Set(3, 4), Set(4, 5), Set(2, 4)),
      Array(Set(0, 1, 2)),
      Array(Set(0, 1, 2), Set(1)),
      Array(Set(0, 1, 2)),
      Array(Set(0, 1, 2), Set(0), Set(1), Set(2)),
      Array(Set(0, 1, 2, 3, 4), Set(1), Set(4), Set(3)),
      Array(Set(0, 1, 2), Set(0), Set(2)),
      Array(Set(1), Set(1), Set(5, 6)),
      Array(Set(8, 16), Set(16, 11), Set(7, 8), Set(16), Set(15, 8), Set(11, 7))
    )
  }
}