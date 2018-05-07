package checker

import java.util.function.Function

import Conversions.checkerToScalaFunction

class ACPruning(checker: Array[Int] => Boolean) extends Filter {

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    applyACPruning(variables)
  }

  @throws[NoSolutionException]
  def applyACPruning(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val sol = cartesianProduct(variables)
    if (sol.isEmpty)
      throw new NoSolutionException
    val result = toDomainsAC(sol)
    result
  }

  //Cartesian product logic
  @throws[NoSolutionException]
  private[this] def cartesianProduct(variables: Array[Set[Int]]): Stream[List[Int]] = {
    if (variables.length < 1)
      throw new NoSolutionException
    var str = firstStream(variables.last).filter(x => checker(x.toArray))
    if (variables.length == 1)
      return str
    for (i <- variables.length - 2 to 0 by -1) {
      str = nthStream(variables(i), str).filter(x => checker(x.toArray))
    }
    str
  }

  private[this] def firstStream(variable: Set[Int]): Stream[List[Int]] = {
    variable.map(x => List(x)).toStream
  }

  private[this] def nthStream(variable: Set[Int], previousStream: Stream[List[Int]]): Stream[List[Int]] = {
    if (previousStream.isEmpty) return Stream.empty[List[Int]]
    val solution = previousStream.head
    val str = (variable.last :: solution) #:: nthStream(variable, previousStream.tail)
    val vars = variable - variable.last
    perOneValueStreamConstructor(solution, vars, str)

  }

  private[this] def perOneValueStreamConstructor(solution: List[Int], variable: Set[Int], str: Stream[List[Int]]): Stream[List[Int]] = {
    if (variable.isEmpty) return str
    val current = variable.last
    (current :: solution) #:: perOneValueStreamConstructor(solution, variable - current, str)
  }

  //Retrieval of the domains from their solutions
  private[this] def toDomainsAC(solutions: Stream[List[Int]]): Array[Set[Int]] = {
    val variables: Array[Set[Int]] = Array.fill(solutions.head.size)(Set.empty)
    solutions.foreach { sol =>
      for (i <- variables.indices) {
        variables(i) += sol(i)
      }
    }
    variables
  }

  //Retrieval of the domains filtered
  /*protected[this] def toDomainsAC(solutions: Array[Array[Int]]): Array[Set[Int]] = {
    val variables: Array[Set[Int]] = Array.fill(solutions(0).length)(Set.empty)
    solutions.foreach { sol =>
      for (i <- variables.indices) {
        variables(i) += sol(i)
      }
    }
    variables
  }

  // applyAC that seems to work in 8 lines! :)
  // inconvenient : generate all solutions and then filter. So, not slower but uses
  // a lot more memory
  def applyACPruning(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val solutions: Array[Array[Int]] = variables.foldLeft(Array[Array[Int]](Array()))((acc: Array[Array[Int]], x: Set[Int]) => {
      var sol: Array[Array[Int]] = Array[Array[Int]]()
      x.foreach(elem => acc.foreach(y => sol = sol :+ (y :+ elem)))
      sol.filter(x => checker(x))
    })
    if (solutions.isEmpty) return Array.fill(variables.length)(Set[Int]())
    toDomainsAC(solutions)
  }*/

}
