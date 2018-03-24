package checker.constraints

import checker._
import org.scalacheck.Prop.forAll

object Gcc extends Checker {

  private[this] var values: Array[Int] = Array()

  private[this] def setGen(): Unit = {
    gen.baseRange = (0, 2)
    gen.baseDensity = 0.8
    gen.setNVar(8)
    gen.baseRange = (0, 8)
    gen.baseDensity = 2.0 / 8.0
    gen.addNVar(3)
  }

  def checkAC(constraint: (Array[Set[Int]], Array[Int]) => Array[Set[Int]]): Unit = {
    val check: Array[Set[Int]] => Boolean = checkConstraint(_, constraint(_, Array(0, 1, 2)))
    values = Array(0, 1, 2)
    setGen()
    forAll(gen.gen) { variables =>
      variables.isEmpty || checkEmpty(variables) || variables.length < 4 || check(variables.toArray)
    }.check(gen.getTestParameters)
    LimitCases.gccLimitCases.foreach { limit =>
      values = limit._2
      checkConstraint(limit._1, constraint(_, limit._2))
    }
  }

  /*
   * TODO ask if count variables should be incremental in their representing value or not
   * This function checks the solution respect the gcc constraint when
   *  solution[0..solution.length-values.length-1] correspond to the assignment variables
   *  solution[solution.length-values.length..solution.length] corespond to the count variables
   *  where values represent the value corresponding to their respective count variable.
   *  each count variable correspond to the value of the previous count variable's value +1
   *  this function cannot be used to test an incomplete solution
   */
  def gccChecker(solution: Array[Int], values: Array[Int], count: Array[Set[Int]]): Boolean = {
    var valuesCount: Map[Int, Int] = Map()
    values.foreach { v => valuesCount = valuesCount + (v -> 0) }
    solution.foreach { x =>
      if (valuesCount.contains(x)) {
        valuesCount = valuesCount.updated(x, valuesCount(x) + 1)
      }
    }
    for (i <- values.indices) {
      if (!count(i).contains(valuesCount(values(i)))) return false
    }
    true
  }

  private[this] def gccToDomainsAC(solutions: Array[Array[Int]], values: Array[Int], count: Array[Set[Int]]): Array[Set[Int]] = {
    if (solutions.length == 0) throw NoSolutionException()
    val n: Int = solutions(0).length
    val variables: Array[Set[Int]] = Array.fill(n + count.length)(Set.empty)
    solutions.foreach { sol =>
      var valuesCount: Map[Int, Int] = Map()
      values.foreach { v => valuesCount = valuesCount + (v -> 0) }
      for (i <- sol.indices) {
        variables(i) += sol(i)
        if (valuesCount.contains(sol(i))) {
          valuesCount = valuesCount.updated(sol(i), valuesCount(sol(i)) + 1)
        }
      }
      for (i <- count.indices) {
        variables(i + n) += valuesCount(values(i))
      }
    }
    variables
  }

  def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val n: Int = values.length
    val assignments: Array[Set[Int]] = variables.dropRight(n)
    val count: Array[Set[Int]] = variables.drop(variables.length - n)
    val sols: Array[Array[Int]] = Constraint.solutions(assignments).filter(x => gccChecker(x, values, count))
    gccToDomainsAC(sols, values, count)
  }

}
