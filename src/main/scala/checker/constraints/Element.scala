package checker.constraints

import checker._

import scala.collection.mutable


class Element extends Constraint2 {
  gen.addNVar(5)
  gen.addVar(0.2, (-1, 10))
  gen.addVar(0.1, (-11, 11))

  override def checker(solution: Array[Int]): Boolean = {
    val X: Array[Int] = solution.dropRight(2)
    val i: Int = solution(solution.length - 2)
    val v: Int = solution(solution.length - 1)
    if (i > X.length) return false
    X(i) == v
  }

  override protected[this] def applyConstraintAC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    elementAC(variables)
  }

  /*
  * element constraint : element(X,i,v). Here, the i and v are put at the end of the vector for convenient purposes
  * The variables x in argument represents the set of variables and x(x.length-1) = v while x(x.length-2) = i
  */
  @throws[NoSolutionException]
  def elementAC(x: Array[Set[Int]]): Array[Set[Int]] = {
    val X: Array[Set[Int]] = x.dropRight(2)
    val i: Set[Int] = x(x.length - 2)
    val v: Set[Int] = x(x.length - 1)

    var reducedDomainv: mutable.Set[Int] = mutable.Set()
    var reducedDomaini: mutable.Set[Int] = mutable.Set()

    for (value <- v) {
      for (index <- i) {
        if (index >= 0 && index < X.length && X(index).contains(value)) {
          reducedDomainv += value
          reducedDomaini += index
        }
      }
    }

    if (reducedDomainv.isEmpty || reducedDomaini.isEmpty)
      throw new NoSolutionException

    if (reducedDomaini.size == 1) {
      val set: mutable.Set[Int] = reducedDomainv.clone()
      X(reducedDomaini.head) = set.toSet
    }

    X ++ Array(reducedDomaini.toSet, reducedDomainv.toSet)
  }

}