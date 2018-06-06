package checker

import Conversions._

/**
  * This object contains different checker functions
  * representing different constraints for the cons-
  * trustors of the filterings and prunings classes.
  */
object Checkers {
  /**
    * This checker always return true.
    */
  def trueConstraint(instantiation: Array[Int]): Boolean = true

  /**
    * This checker always return false.
    */
  def falseConstraint(instantiation: Array[Int]): Boolean = false

  /**
    * This checker always return true if all the elements
    * are different.
    */
  def allDifferent(instantiation: Array[Int]): Boolean = instantiation.toSet.size == instantiation.length

  /**
    * @return the checker function for the allDifferent constraint
    */
  def allDifferent(): Array[Int] => Boolean = x => allDifferent(x)

  /**
    * This checker function consider the instantiation to have the format:
    * [x_1 x_2 x_3 ... x_n i v]
    *
    * @return x_i==v
    */
  def element(instantiation: Array[Int]): Boolean = {
    if (instantiation.length <= 2) return false
    val X: Array[Int] = instantiation.dropRight(2)
    val i: Int = instantiation(instantiation.length - 2)
    val v: Int = instantiation(instantiation.length - 1)
    element(X, i, v)
  }

  /**
    * @return the checker function for the element constraint with the
    *         supposed format of the above function
    */
  def element(): Array[Int] => Boolean = x => element(x)

  /** This function can be used as a checker by fixing the values of
    * 'i' and 'v'.
    *
    * @param X : an arbitrary array of integer values
    * @param i : an index of X
    * @param v : an arbitrary value
    * @return X(i)==v
    */
  def element(X: Array[Int], i: Int, v: Int): Boolean = {
    if (i < 0 || i >= X.length) return false
    X(i) == v
  }

  /**
    * @param i : an index
    * @param v : a value
    * @return a checker function for the element constraint with the
    *         index and value fixed at 'i' and 'v'.
    */
  def element(i: Int, v: Int): Array[Int] => Boolean = x => element(x, i, v)

  /**
    * @param constant : the constant of the sum constraint
    * @param operator : the operator representing the relation between
    *                 the sum and the constant. The existing relations
    *                 are : '=', '>=', '>', '<', '<=' and '!='.
    * @return sum(instantiation) operator constant. If the operator and
    *         the constant are not given, it returns sum(instantiation) == 0.
    *         a wrong operator makes it always returns false.
    */
  def sum(instantiation: Array[Int], constant: Int = 0, operator: String = "="): Boolean = {
    if (operator.equals("=")) return instantiation.sum == constant
    else if (operator.equals("!=")) return instantiation.sum != constant
    else if (operator.equals(">")) return instantiation.sum > constant
    else if (operator.equals("<")) return instantiation.sum < constant
    else if (operator.equals(">=")) return instantiation.sum >= constant
    else if (operator.equals("<=")) return instantiation.sum <= constant
    false
  }

  /**
    * @return a checker function for the sum constraint given an operator
    *         and a constant.
    */
  def sum(constant: Int, operator: String): Array[Int] => Boolean = x => sum(x, constant, operator)

  /**
    * @param table : set of all the accepted instantiations
    * @return true if instantiation is one of the accepted instantiations.
    */
  def table(instantiation: Array[Int], table: Set[Array[Int]]): Boolean = {
    table.exists(x => x.sameElements(instantiation))
  }

  /**
    * @param t : set of all the accepted instantiations
    * @return a checker function returning true if its given argument is
    *         one of the accepted instantiations.
    */
  def table(t: Set[Array[Int]]): Array[Int] => Boolean = x => table(x, t)

  /**
    * Same function as the one above but for Java objects.
    */
  def table(t: java.util.Set[Array[Integer]]): Array[Int] => Boolean =
    x => table(x, t)

  /**
    * @param instantiation : array containing first the variables then the occurrences
    * @param values        array of the same length as the number of occurrence variables.
    * @return true if the GCC constraint is respected.
    */
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

  /**
    * @param values : array of the values represented by the occurences.
    * @return a checker for the GCC constraint considering the
    *         occurrences as variables.
    */
  def gccVar(values: Array[Int]): Array[Int] => Boolean = x => gccVar(x, values)

  /**
    * @return for each index 'i' of 'values', 'instantiation' must contain
    *         'occurrences(i)' times 'values(i)'.
    */
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

  /**
    * @return a checker function for the GCC constraint given occurrences and values.
    */
  def gcc(occurrences: Array[Int], values: Array[Int]): Array[Int] => Boolean = x => gcc(x, occurrences, values)
}
