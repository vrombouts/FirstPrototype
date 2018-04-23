package checker.constraints

import checker._

import scala.collection.mutable


class Element extends Constraint {
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

  override def limitCases() : Array[Array[Set[Int]]] = {
    Array(
      Array(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3), Set(0, 1, 2, 3), Set(1, 2)), // test with one i > x.size
      Array(Set(1, 2, 3), Set(1, 2, 3), Set(0, 1), Set(1, 2, 3)), // test with nothing to remove
      Array(Set(1, 2, 3), Set(1, 2), Set(4, 5), Set(0, 1, 2), Set(4, 5)), // test with only one variable s.t. x(i)=v
      Array(Set(1, 2, 3), Set(1, 2), Set(0, 1, 2), Set(1, 2, 4)), // test with values of v to remove
      Array(Set(1, 2, 3), Set(1, 2), Set(1, 2), Set(1), Set(1, 2, 3)), //test with i of size 1
      Array(Set(1, 2, 3), Set(1, 2), Set(1, 2), Set(0, 1, 2), Set(2)), //test with v of size 1
      Array(Set(1, 2, 3), Set(1, 2), Set(1, 2), Set(0), Set(2)), //test with v and i of size 1
      Array(Set(1, 2, 3), Set(1, 2), Set(4), Set(2)), // test with no sol because of i
      Array(Set(1, 2, 3), Set(1, 2), Set(0, 1), Set(4, 5, 6)), // test with no sol because of v
      Array(Set(1, 2, 3), Set(1, 2), Set(1, 2, 4), Set(0, 1, 2), Set(1, 2)), // normal case with more than one match
      Array(Set(1), Set(1), Set(1), Set(1), Set(0, 1, 2, 3), Set(1)), //test with all matches the element constraint
      Array(Set(0), Set(-2), Set(-4, -2), Set(1, 2), Set(-2, -3))
    )
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