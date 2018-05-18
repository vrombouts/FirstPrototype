package checker

import java.util.function.Function

import Conversions.checkerToScalaFunction

class ACPruning(checker: Array[Int] => Boolean) extends Filter {

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    applyACPruning(variables)
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

  @throws[NoSolutionException]
  def applyACPruning(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if(variables.isEmpty) throw new NoSolutionException
    val solutions: Set[Array[Int]] = variables.foldLeft(Set[Array[Int]](Array()))((acc: Set[Array[Int]], x: Set[Int]) => {
      var sol: Set[Array[Int]] = Set[Array[Int]]()
      x.foreach(elem => acc.foreach(y => sol += y :+ elem))
      sol.filter(x => checker(x))
    })
    if (solutions.isEmpty) throw new NoSolutionException
    toDomains(solutions)
  }

}
