package checker

import Conversions._

object Checkers {
  def trueConstraint(solution: Array[Int]): Boolean = true

  def falseConstraint(solution: Array[Int]): Boolean = false

  def allDifferent(solution: Array[Int]): Boolean = solution.toSet.size == solution.length

  def allDifferent(): Array[Int] => Boolean = x => allDifferent(x)

  def element(solution: Array[Int]): Boolean = {
    if (solution.length <= 2) return false
    val X: Array[Int] = solution.dropRight(2)
    val i: Int = solution(solution.length - 2)
    val v: Int = solution(solution.length - 1)
    element(X, i, v)
  }

  def element(): Array[Int] => Boolean = x => element(x)

  def element(solution: Array[Int], i: Int, v: Int): Boolean = {
    if (i < 0 || i >= solution.length) return false
    solution(i) == v
  }

  def element(i: Int, v: Int): Array[Int] => Boolean = x => element(x, i, v)

  def sum(solution: Array[Int], constant: Int = 0, operator: String = "="): Boolean = {
    if (operator.equals("=")) return solution.sum == constant
    else if (operator.equals("!=")) return solution.sum != constant
    else if (operator.equals(">")) return solution.sum > constant
    else if (operator.equals("<")) return solution.sum < constant
    else if (operator.equals(">=")) return solution.sum >= constant
    else if (operator.equals("<=")) return solution.sum <= constant
    false
  }

  def sum(constant: Int, operator: String): Array[Int] => Boolean = x => sum(x, constant, operator)

  def table(solution: Array[Int], table: Set[Array[Int]]): Boolean = {
    table.exists(x => x.sameElements(solution))
  }

  def table(t: Set[Array[Int]]): Array[Int] => Boolean = x => table(x, t)

  def table(t: java.util.Set[Array[Integer]]): Array[Int] => Boolean =
    x => table(x, t)

  def gccVar(solution: Array[Int], values: Array[Int]): Boolean = {
    if (solution.length < values.length) return false
    var valuesCount: Map[Int, Int] = Map()
    val variables = solution.dropRight(values.length)
    val occurences = solution.drop(solution.length - values.length)
    values.foreach { v => valuesCount = valuesCount + (v -> 0) }
    variables.foreach { x =>
      if (valuesCount.contains(x)) {
        valuesCount = valuesCount.updated(x, valuesCount(x) + 1)
      }
    }
    for (i <- values.indices) {
      if (!(occurences(i) == valuesCount(values(i)))) return false
    }
    true
  }

  def gccVar(values: Array[Int]): Array[Int] => Boolean = x => gccVar(x, values)

  def gcc(solution: Array[Int], occurences: Array[Int], values: Array[Int]): Boolean = {
    assert(occurences.length == values.length)
    if (solution.length < occurences.sum) return false
    var valuesCount: Map[Int, Int] = Map()
    values.foreach { v => valuesCount = valuesCount + (v -> 0) }
    solution.foreach { x =>
      if (valuesCount.contains(x)) {
        valuesCount = valuesCount.updated(x, valuesCount(x) + 1)
      }
    }
    for (i <- values.indices) {
      if (!(occurences(i) == valuesCount(values(i)))) return false
    }
    true
  }

  def gcc(occurences: Array[Int], values: Array[Int]): Array[Int] => Boolean = x => gcc(x, occurences, values)
}
