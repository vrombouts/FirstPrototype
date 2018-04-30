package checker.constraints

import checker.{Statistics, StrictStatistics, UnstrictStats}
import checker.constraints.incremental.{BranchOp, Incremental}
import org.scalacheck.Prop.forAll

/*
 * This trait contains the check functions to check a constraint
 */
trait Checks extends Static with Incremental{

  var statistic: Array[Statistics] = Array(new UnstrictStats(nbBranchOp, "check"), new StrictStatistics(nbBranchOp, "AC"), new StrictStatistics(nbBranchOp, "BC")) // basically, it's unstrict stats
  def stats: Statistics = statistic(propagation)
  private[this] var checkFunction: Array[Int] => Boolean = _

  protected[this] def checker(solution: Array[Int]): Boolean = {
    checkFunction(solution)
  }

  protected[this] def limitCases: Array[Array[Set[Int]]] = Array()

  def checkAC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = AC
    forAllCheck(filteringTested)
    stats.setGenerator(gen)
    stats.print
  }

  def checkBC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = BC
    forAllCheck(filteringTested)
    stats.setGenerator(gen)
    stats.print
  }

  def check(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = notSpecified
    forAllCheck(filteringTested)
    stats.setGenerator(gen)
    stats.print
  }

  def checkAC(init: Array[Set[Int]] => Array[Set[Int]],
              filtering: BranchOp => Array[Set[Int]],
              checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = AC
    forAllCheck(init, filtering)
    stats.setGenerator(gen)
    stats.print(isInc = true)
  }

  def checkBC(init: Array[Set[Int]] => Array[Set[Int]],
              filtering: BranchOp => Array[Set[Int]],
              checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = BC
    forAllCheck(init, filtering)
    stats.setGenerator(gen)
    stats.print(isInc = true)
  }

  def check(init: Array[Set[Int]] => Array[Set[Int]],
            filtering: BranchOp => Array[Set[Int]],
            checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = notSpecified
    forAllCheck(init, filtering)
    stats.setGenerator(gen)
    stats.print(isInc = true)
  }


  private[this] def forAllCheck(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    forAll(gen.gen) { x =>
      x.isEmpty || (x.length<gen.getNbVars) || checkEmpty(x) || checkConstraint(x.toArray, filteringTested)
    }.check(gen.getTestParameters)
    limitCases.forall(x => checkConstraint(x, filteringTested))
  }

  private[this] def forAllCheck(init: Array[Set[Int]] => Array[Set[Int]],
                                filtering: BranchOp => Array[Set[Int]]): Unit = {
    forAll(gen.gen) { x =>
      x.isEmpty || (x.length<gen.getNbVars) || checkEmpty(x) || checkConstraint(x.toArray, init, filtering)
    }.check(gen.getTestParameters)
    limitCases.forall(x => checkConstraint(x, init, filtering))
  }

  private[this] def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

}

class Constraint extends Checks with ACBasic with BCBasic

class ConstraintPruning extends Checks with ACPruning with BCPruning