package checker.constraints

import checker._
import org.scalacheck.Prop.forAll

import scala.collection.mutable

object Element extends Checker {

  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = elementAC(variables)

  def checkAC(constraint: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    val check: (Array[Set[Int]]) => Boolean = checkConstraint(_, constraint)
    gen.addNVar(5)
    gen.addVar(0.2, (-1, 10))
    gen.addVar(0.1, (-11, 11))
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || check(x.toArray) //x.length<2
    }.check(gen.getTestParameters)
    LimitCases.elementLimitCases.foreach(limitCase => {
      check(limitCase)
    })
  }

  override def printer(returnValues: Array[Array[Set[Int]]]): Unit = {
    val initial: Array[Set[Int]] = returnValues(0)
    val trueReduced: Array[Set[Int]] = returnValues(1)
    val reduced: Array[Set[Int]] = returnValues(2)
    val init = initial.dropRight(2).toList
    val initI = initial(initial.length - 2)
    val initV = initial(initial.length - 1)
    val tr = trueReduced.dropRight(2).toList
    val trI = trueReduced(trueReduced.length - 2)
    val trV = trueReduced(trueReduced.length - 1)
    val r = reduced.dropRight(2).toList
    val rI = reduced(reduced.length - 2)
    val rV = reduced(reduced.length - 1)
    val strFail: String = "failed with X domains: " + init + "\n i domain: " + initI + "\n v domain: " + initV
    val strShould: String = "you should have: " + tr + "\n i domain: " + trI + "\n v domain: " + trV
    val strHave: String = "but you had: " + r + "\n i domain: " + rI + "\n v domain: " + rV
    if (reduced.isEmpty && trueReduced.nonEmpty) {
      println(strFail)
      println(strShould)
      println("but you returned no solution can be found")
    } else if (reduced.nonEmpty && trueReduced.isEmpty) {
      println(strFail)
      println("you should not have any solutions")
      println(strHave)
    } else if (reduced.nonEmpty && trueReduced.nonEmpty) {
      println(strFail)
      println(strShould)
      println(strHave)
    }
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

  /*
   * element(x,i,v) constraint
   * Important to note that the solution array contains the variables x followed by i and v
   */
  def elementConstraint(solution: Array[Int]): Boolean = {
    val X: Array[Int] = solution.dropRight(2)
    val i: Int = solution(solution.length - 2)
    val v: Int = solution(solution.length - 1)
    if (i > X.length) return false
    X(i) == v
  }

}
