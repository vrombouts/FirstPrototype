package checker.constraints

import checker._
import Conversions._

class Table(table: Set[Array[Int]]) extends Constraint2 with ACBasic {
  def this(table: java.util.Set[Array[Integer]]) = this(tableToScala(table))

  private[this] val min: Int = table.map(x => x.min).min
  private[this] val max: Int = table.map(x => x.max).max
  gen.setNVar(table.last.length)
  gen.setRangeForAll(min, max)
  gen.setDensityForAll(3 * 1 / (max - min))

  override def checker(solution: Array[Int]): Boolean = {
    table.exists(x => x.sameElements(solution))
  }

  private[this] def transformTableInDom(tab: Set[Array[Int]]): Array[Set[Int]] = {
    var length: Int = 0
    tab.foreach(x => if (x.length > length) length = x.length)
    var res: Array[Set[Int]] = Array.fill(length)(Set())
    for (sol <- tab) {
      for (i <- sol.indices) {
        res(i) += sol(i)
      }
    }
    res
  }

  override def limitCases(): Array[Array[Set[Int]]] = {
    var result: Array[Array[Set[Int]]] = Array()
    var maxValue: Int = 0
    if (table.isEmpty) return Array()
    table.foreach(x => x.foreach(y => if (y > maxValue) maxValue = y))
    val outVal: Int = maxValue + 1
    val domains: Array[Set[Int]] = transformTableInDom(table)
    var test1: Array[Set[Int]] = Array() //basic test where values are filtered
    for (i <- 0 until domains.length - 1)
      test1 = test1 :+ domains(i)
    test1 = test1 :+ domains(domains.length - 1) + outVal
    result = result :+ test1
    var test2: Array[Set[Int]] = Array() //no value are filtered
    for (i <- domains.indices)
      test2 = test2 :+ domains(i)
    result = result :+ test2
    var test3: Array[Set[Int]] = Array()// todo
    for (i <- 0 until domains.length - 1) {
      test3 = test3 :+ domains(i)
    }
    if (!test3.exists(x => x.isEmpty)) result = result :+ test3
    result
    /*(Array(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3)),
      Set(Array(1, 1, 1), Array(2, 2, 2), Array(3, 3, 3), Array(4, 4, 4))), //not a growing domain/line in table within no domains
    (Array(Set(1, 2), Set(2, 3)),
      Set(Array(1, 3), Array(3, 1))), //test if domains not melanged(ex:returning Set(1,2,3),Set(1,2,3))
    (Array(Set(0, 1, 2), Set(0, 1, 2), Set(0, 1, 2)),
      Set(Array(0, 1, 4), Array(4, 2, 0), Array(1, 4, 2))), //one value out of domains per line in table
    //(Array(Set(Integer.MAX_VALUE, Integer.MIN_VALUE),Set(Integer.MAX_VALUE, Integer.MIN_VALUE)),
    //Set(Array(-10,10),Array(-100,100),Array(1000,-1000))),//no problems with overflow,
    (Array(Set(1), Set(2), Set(10), Set(40)),
      Set(Array(1, 2, 10, 40), Array(40, 10, 2, 1), Array(0, 0, 0, 0))) // No problem with single value and ordering
  )*/
  }

  override def applyConstraintAC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    var solutions: Set[Array[Int]] = Set.empty
    table.foreach(solution => if (possibleWith(variables, solution)) solutions += solution)
    if (solutions.isEmpty) throw new NoSolutionException
    toDomainsAC(solutions.toArray)
  }

  private[this] def possibleWith(variables: Array[Set[Int]], solution: Array[Int]): Boolean = {
    for (i <- solution.indices) {
      if (!variables(i).contains(solution(i))) return false
    }
    true
  }
}