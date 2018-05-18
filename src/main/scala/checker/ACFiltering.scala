package checker

import scala.collection.mutable
import java.util.function.Function
import Conversions.checkerToScalaFunction

class ACFiltering(checker: Array[Int] => Boolean) extends Filter {

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    filterAC(variables)
  }

  protected[this] def toDomains(solutions: Set[Array[Int]]): Array[Set[Int]] = {
    val variables: Array[Set[Int]] = Array.fill(solutions.head.length)(Set.empty)
    solutions.foreach { sol =>
      for (i <- variables.indices) {
        variables(i) += sol(i)
      }
    }
    variables
  }

  // applyAC that seems to work in 8 lines! :)
  // inconvenient : generate all solutions and then filter. So, uses
  // a lot more memory
  // and slower :(
  @throws[NoSolutionException]
  def filterAC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if (variables.isEmpty) throw new NoSolutionException
    val solutions: Set[Array[Int]] = variables.foldLeft(Set[Array[Int]](Array()))((acc, x) => {
      var set: Set[Array[Int]] = Set[Array[Int]]()
      x.foreach(elem => acc.foreach(y => set += y :+ elem))
      set
    }).filter(x => checker(x))
    if (solutions.isEmpty) throw new NoSolutionException
    toDomains(solutions)
  }

}
