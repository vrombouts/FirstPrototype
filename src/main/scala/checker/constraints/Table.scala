package checker.constraints

import checker._
import Conversions._

class Table(table: Set[Array[Int]]) extends Constraint with ACBasic {
  def this(table: java.util.Set[Array[Integer]]) = this(tableToScala(table))

  private[this] val min: Int = table.map(x=>x.min).min
  private[this] val max: Int = table.map(x=>x.max).max
  gen.setNVar(table.last.length)
  gen.setRangeForAll(min,max)
  gen.setDensityForAll(3*1/(max-min))

  override def checker(solution: Array[Int]): Boolean = {
    table.exists(x => x.sameElements(solution))
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