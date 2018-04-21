package checker.constraints

class AllDifferent extends Constraint2 {
  gen.setRangeForAll((-5, 5))
  gen.setDensityForAll(0.3)
  gen.setNVar(7)

  override def checker(solution: Array[Int]): Boolean = solution.toSet.size == solution.length

}