package checker.constraints

import checker._


class Gcc(values: Array[Int]) extends Constraint with ACBasic {
  setGen()

  private[this] def setGen(): Unit = {
    //TODO: be more in accordance to values
    gen.baseRange = (values.min, values.max)
    gen.baseDensity = 3 / values.length
    gen.setNVar(8)
    gen.baseRange = (0, 8)
    gen.baseDensity = 2.0 / 8.0
    gen.addNVar(values.length)
  }

  override def checker(solution: Array[Int]): Boolean = {
    if(solution.length<values.length) return false
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

  private[this] def createVars(array: Array[Int], nb: Int): Array[Set[Int]] = {
    val domain: Set[Int] = array.toSet
    Array.fill(nb)(domain)
  }

  private[this] def createCounts(array: Array[Int], range: (Int, Int)): Array[Set[Int]] = {
    var possibleValues: Set[Int] = Set()
    for (i <- range._1 to range._2) {
      possibleValues += i
    }
    Array.fill(array.length)(possibleValues)
  }

  override def limitCases(): Array[Array[Set[Int]]] = {
    if (values.isEmpty) return Array()
    println("values " + values.toList)

    def test(nb: Int, range: (Int, Int)): Array[Set[Int]] = createVars(values, nb) ++ createCounts(values, range)

    var test1: Array[Set[Int]] = test(2, (0, 2)) // filtering correctly the count variables
    println(test1.toList)

    val test2: Array[Set[Int]] = test(values.length, (0, 1)) // allDifferent constraint
    println(test2.toList)
    val test3: Array[Set[Int]] = test(values.length, (1, 1)) // count variables are set to 1
    println(test3.toList)
    val test4: Array[Set[Int]] = test(values.length, (0, 0)) // no solution because of counts
    println(test4.toList)
    val test5: Array[Set[Int]] = Array.fill(values.length)(Set(values(0))) ++ createCounts(values, (0, values.length)) // test with all variables being equal to the same val => filtering counts correctly
    println(test5.toList)
    var result: Array[Array[Set[Int]]] = Array(test1, test2, test3, test4, test5)
    if (values.length > 1) {
      val test6: Array[Set[Int]] = test(values.length, (1, 1)) // permutation test
      test6(1) = Set(test6(0).min)
      test6(0) -= test6(1).max
      if (test6.forall(x => x.nonEmpty)) result = result :+ test6
      println(test6.toList)
    }
    result

    /* Array(
       (Array(Set(0,1), Set(0,1), Set(0, 1, 2), Set(0, 1)),Array(0,1)), //pruning count correctly
       (Array(Set(0, 1, 2), Set(0, 1, 2), Set(0, 1, 2), Set(0, 1), Set(0, 1), Set(0, 1)), Array(0, 1, 2)), //alldifferent
       (Array(Set(1, 2, 3, 4), Set(1, 2), Set(2, 1), Set(3, 4), Set(1), Set(1), Set(1), Set(1)), Array(1, 2, 3, 4)) //permutation
       //TODO add limit cases
     )*/
  }

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
