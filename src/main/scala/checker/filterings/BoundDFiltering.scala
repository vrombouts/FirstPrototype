package checker.filterings

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker.{Filter, NoSolutionException}

class BoundDFiltering(checker: Array[Int] => Boolean) extends Filter {

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    var changed: Boolean = true
    while (changed) {
      changed = false
      for (i <- variables.indices) {
        if (changeBounds(i, variables)) changed = true
      }
    }
    variables
  }


  def changeBounds(i: Int, variables: Array[Set[Int]]): Boolean = {
    Array(variables(i).min, variables(i).max).foldLeft(false) { (acc, value) =>
      if (!findASolution(variables, i, value)) {
        if (variables(i).size == 1) throw NoSolutionException()
        variables(i) -= value
        true
      } else acc
    }
  }

  //Generation of all the solutions to find a solution accepted by `checker´
  private[this] def findASolution(variables: Array[Set[Int]], index: Int, value: Int): Boolean = {
    val currentSol: Array[Int] = Array.fill(variables.length)(0)
    currentSol(index) = value

    def setIthVariable(currentIndex: Int): Boolean = {
      if (currentIndex == index) return setIthVariable(currentIndex + 1)
      if (currentIndex == variables.length) return checker(currentSol)
      for (i <- variables(currentIndex)) {
        currentSol(currentIndex) = i
        if (setIthVariable(currentIndex + 1))
          return true
      }
      false
    }

    setIthVariable(0)
  }

}
