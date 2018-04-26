package checker.constraints

import checker.{Base, NoSolutionException}

trait ACPruning extends Base {
  override protected[this] def applyConstraintAC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    applyACPruning(variables)
  }

  // Applying AC with pruning //
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
}
