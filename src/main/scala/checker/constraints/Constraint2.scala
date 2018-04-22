package checker.constraints

import checker.{Statistics, StrictStatistics, UnstrictStats}
import checker.constraints.incremental.{BranchOp, Incremental}
import org.scalacheck.Prop.forAll

class Constraint2 extends Static with Incremental with ACFiltering with BCFiltering{
  var stats: Statistics = new UnstrictStats // basically, it's unstrict stats
  private[this] var checkFunction: Array[Int] => Boolean = {_=>{true}} // initial true checker

  protected[this] def checker(solution: Array[Int]): Boolean = {
    checkFunction(solution)
  }

  protected[this] def applyConstraintSimple(variables: Array[Set[Int]]): Array[Set[Int]] = {
    applyAC(variables)
  }

  protected[this] def limitCases() : Array[Array[Set[Int]]] = Array()

  def checkAC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = AC
    stats = new StrictStatistics
    forAllCheck(filteringTested)
    //TODO: add simple case limit possible for all constraints
    stats.setGenerator(gen)
    stats.print
  }

  def checkBC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = BC
    stats = new StrictStatistics
    forAllCheck(filteringTested)
    //TODO: add simple case limit possible for all constraints
    stats.setGenerator(gen)
    stats.print
  }

  def check(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    stats = new UnstrictStats
    forAllCheck(filteringTested)
    stats.setGenerator(gen)
    stats.print
  }

  def checkAC(init: Array[Set[Int]] => Array[Set[Int]],
              filtering: BranchOp => Array[Set[Int]],
              checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = AC
    stats = new StrictStatistics(nbBranchOp)
    forAllCheck(init, filtering)
    stats.setGenerator(gen)
    stats.print(isInc = true)
  }

  def checkBC(init: Array[Set[Int]] => Array[Set[Int]],
              filtering: BranchOp => Array[Set[Int]],
              checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    propagation = BC
    stats = new StrictStatistics(nbBranchOp)
    forAllCheck(init, filtering)
    stats.setGenerator(gen)
    stats.print(isInc = true)
  }

  def check(init: Array[Set[Int]] => Array[Set[Int]],
            filtering: BranchOp => Array[Set[Int]],
            checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    stats = new UnstrictStats(nbBranchOp)
    forAllCheck(init, filtering)
    stats.setGenerator(gen)
    stats.print(isInc = true)
  }



  private[this] def forAllCheck(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringTested)
    }.check(gen.getTestParameters)
    limitCases().forall(x => checkConstraint(x, filteringTested))
  }

  private[this] def forAllCheck(init: Array[Set[Int]] => Array[Set[Int]],
                            filtering: BranchOp => Array[Set[Int]]): Unit = {
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, init, filtering)
    }.check(gen.getTestParameters)
    limitCases().forall(x => checkConstraint(x, init, filtering))
  }

  private[this] def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach { x => if (x.isEmpty) return true }
    false
  }

}

class BasicConstraint extends Constraint2 with ACBasic