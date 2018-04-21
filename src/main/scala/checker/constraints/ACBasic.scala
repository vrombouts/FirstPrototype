package checker.constraints

import checker.{Base, NoSolutionException}

import scala.collection.mutable

trait ACBasic extends Base {
  override protected[this] def applyConstraintAC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    applyACWithoutPruning(variables)
  }

  def applyACWithoutPruning(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if (variables.isEmpty) throw NoSolutionException()
    val sols: Array[Array[Int]] = solutions(variables).filter(x => checker(x))
    if (sols.isEmpty) throw NoSolutionException()
    toDomainsAC(sols)
  }


  //Generation of all the solutions
  protected[this] def solutions(variables: Array[Set[Int]]): Array[Array[Int]] = {
    val currentSol: Array[Int] = Array.fill(variables.length)(0)
    val result: mutable.Set[Array[Int]] = mutable.Set()
    setIthVariable(variables, 0, currentSol, result)
    result.toArray
  }

  private[this] def setIthVariable(variables: Array[Set[Int]], index: Int, currentSol: Array[Int], result: mutable.Set[Array[Int]]): Unit = {
    for (i <- variables(index)) {
      currentSol(index) = i
      if (index == variables.length - 1) {
        result += currentSol.clone()
      } else {
        setIthVariable(variables, index + 1, currentSol, result)
      }
    }
  }

  //Retrieval of the domains filtered
  private[this] def toDomainsAC(solutions: Array[Array[Int]]): Array[Set[Int]] = {
    val variables: Array[Set[Int]] = Array.fill(solutions(0).length)(Set.empty)
    solutions.foreach { sol =>
      for (i <- variables.indices) {
        variables(i) += sol(i)
      }
    }
    variables
  }

}
