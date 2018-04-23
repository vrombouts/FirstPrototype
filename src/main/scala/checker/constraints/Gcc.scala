package checker.constraints

import checker._
import org.scalacheck.Prop.forAll


class Gcc(values: Array[Int]) extends Constraint2 with ACBasic{
  setGen()

  private[this] def setGen(): Unit = {
    //TODO: be more in accordance to values
    gen.baseRange = (values.min, values.max)
    gen.baseDensity = 3/values.length
    gen.setNVar(8)
    gen.baseRange = (0, 8)
    gen.baseDensity = 2.0 / 8.0
    gen.addNVar(values.length)
  }

  override def checker(solution: Array[Int]): Boolean = {
    var valuesCount: Map[Int, Int] = Map()
    val variables  = solution.dropRight(values.length)
    val occurences = solution.drop(solution.length-values.length)
    values.foreach { v => valuesCount = valuesCount + (v -> 0) }
    variables.foreach { x =>
      if (valuesCount.contains(x)) {
        valuesCount = valuesCount.updated(x, valuesCount(x) + 1)
      }
    }
    for (i <- values.indices) {
      if(!(occurences(i) == valuesCount(values(i)))) return false
    }
    true
  }

  /*override def limitCases(): Array[Array[Set[Int]]] = {
   val set:Set[Int]=values.toSet
    Array(
      (Array(Set(0,1), Set(0,1), Set(0, 1, 2), Set(0, 1)),Array(0,1)), //pruning count correctly
      (Array(Set(0, 1, 2), Set(0, 1, 2), Set(0, 1, 2), Set(0, 1), Set(0, 1), Set(0, 1)), Array(0, 1, 2)), //alldifferent
      (Array(Set(1, 2, 3, 4), Set(1, 2), Set(2, 1), Set(3, 4), Set(1), Set(1), Set(1), Set(1)), Array(1, 2, 3, 4)) //permutation
      //TODO add limit cases
    )
  }*/

  override protected[this] def applyConstraintAC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val n: Int = values.length
    val assignments: Array[Set[Int]] = variables.dropRight(n)
    val count: Array[Set[Int]] = variables.drop(variables.length - n)
    val sols: Array[Array[Int]] = solutions(assignments).filter(x => gccChecker(x, values, count))
    gccToDomainsAC(sols, values, count)
  }


  private[this] def gccChecker(solution: Array[Int], values: Array[Int], count: Array[Set[Int]]): Boolean = {
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

}
