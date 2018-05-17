package checker

import Conversions._

object Checkers {
  def trueConstraint(instantiation: Array[Int]): Boolean = true

  def falseConstraint(instantiation: Array[Int]): Boolean = false

  def allDifferent(instantiation: Array[Int]): Boolean = instantiation.toSet.size == instantiation.length

  def allDifferent(): Array[Int] => Boolean = x => allDifferent(x)

  def element(instantiation: Array[Int]): Boolean = {
    if (instantiation.length <= 2) return false
    val X: Array[Int] = instantiation.dropRight(2)
    val i: Int = instantiation(instantiation.length - 2)
    val v: Int = instantiation(instantiation.length - 1)
    element(X, i, v)
  }

  def element(): Array[Int] => Boolean = x => element(x)

  def element(X: Array[Int], i: Int, v: Int): Boolean = {
    if (i < 0 || i >= X.length) return false
    X(i) == v
  }

  def element(i: Int, v: Int): Array[Int] => Boolean = x => element(x, i, v)

  def sum(instantiation: Array[Int], constant: Int = 0, operator: String = "="): Boolean = {
    if (operator.equals("=")) return instantiation.sum == constant
    else if (operator.equals("!=")) return instantiation.sum != constant
    else if (operator.equals(">")) return instantiation.sum > constant
    else if (operator.equals("<")) return instantiation.sum < constant
    else if (operator.equals(">=")) return instantiation.sum >= constant
    else if (operator.equals("<=")) return instantiation.sum <= constant
    false
  }

  def sum(constant: Int, operator: String): Array[Int] => Boolean = x => sum(x, constant, operator)

  def table(instantiation: Array[Int], table: Set[Array[Int]]): Boolean = {
    table.exists(x => x.sameElements(instantiation))
  }

  def table(t: Set[Array[Int]]): Array[Int] => Boolean = x => table(x, t)

  def table(t: java.util.Set[Array[Integer]]): Array[Int] => Boolean =
    x => table(x, t)

  def gccVar(instantiation: Array[Int], values: Array[Int]): Boolean = {
    if (instantiation.length < values.length) return false
    var valuesCount: Map[Int, Int] = Map()
    val variables = instantiation.dropRight(values.length)
    val occurrences = instantiation.drop(instantiation.length - values.length)
    values.foreach { v => valuesCount = valuesCount + (v -> 0) }
    variables.foreach { x =>
      if (valuesCount.contains(x)) {
        valuesCount = valuesCount.updated(x, valuesCount(x) + 1)
      }
    }
    for (i <- values.indices) {
      if (!(occurrences(i) == valuesCount(values(i)))) return false
    }
    true
  }

  def gccVar(values: Array[Int]): Array[Int] => Boolean = x => gccVar(x, values)

  def gcc(instantiation: Array[Int], occurences: Array[Int], values: Array[Int]): Boolean = {
    assert(occurences.length == values.length)
    if (instantiation.length < occurences.sum) return false
    var valuesCount: Map[Int, Int] = Map()
    values.foreach { v => valuesCount = valuesCount + (v -> 0) }
    instantiation.foreach { x =>
      if (valuesCount.contains(x)) {
        valuesCount = valuesCount.updated(x, valuesCount(x) + 1)
      }
    }
    for (i <- values.indices) {
      if (!(occurences(i) == valuesCount(values(i)))) return false
    }
    true
  }

  def gcc(occurrences: Array[Int], values: Array[Int]): Array[Int] => Boolean = x => gcc(x, occurrences, values)
}
