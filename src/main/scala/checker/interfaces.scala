package checker

import java.util.function.{BiFunction, Function}
import Conversions._
import checker.constraints.incremental.BranchOp
import checker.constraints._

class JCpChecker {
  def checkAC(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    Constraint.checkAC(filtering, checker)
  }

  def checkBC(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    Constraint.checkBC(filtering, checker)
  }

  def check(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
            checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    Constraint.check(filtering, checker)
  }

  def checkAC(init: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              filtering: Function[BranchOp, Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    Constraint.checkAC(init, filtering, checker)
  }

  def checkBC(init: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              filtering: Function[BranchOp, Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    Constraint.checkBC(init, filtering, checker)
  }

  def check(init: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              filtering: Function[BranchOp, Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    Constraint.check(init, filtering, checker)
  }

  def checkAllDifferentAC(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    AllDifferent.checkAC(filtering)
  }

  def checkAllDifferentBC(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    AllDifferent.checkBC(filtering)
  }

  def checkSumEQ(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Sum.checkBC(filtering, Op.equal)
  }

  def checkSumNE(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Sum.checkBC(filtering, Op.different)
  }

  def checkSumLT(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Sum.checkBC(filtering, Op.lesserThan)
  }

  def checkSumLE(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Sum.checkBC(filtering, Op.lesserThanOrEqual)
  }

  def checkSumGT(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Sum.checkBC(filtering, Op.greaterThan)
  }

  def checkSumGE(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Sum.checkBC(filtering, Op.greaterThanOrEqual)
  }

  def checkElementAC(filteringTested: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Element.checkAC(filteringTested)
  }

  def checkTableAC(tableFiltering: BiFunction[Array[java.util.Set[Integer]], java.util.Set[Array[Integer]], Array[java.util.Set[Integer]]]): Unit = {
    Table.checkAC(tableFiltering)
  }
}

object ScCpChecker {
  def checkAC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    Constraint.checkAC(filteringTested, checker)
  }

  def checkBC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    Constraint.checkBC(filteringTested, checker)
  }

  def checkAllDifferentAC(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    AllDifferent.checkAC(filteringTested)
  }

  def checkAllDifferentBC(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    AllDifferent.checkBC(filteringTested)
  }

  def checkSumEQ(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    Sum.checkBC(filteringTested, Op.equal)
  }

  def checkSumNE(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    Sum.checkBC(filteringTested, Op.different)
  }

  def checkSumLT(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    Sum.checkBC(filteringTested, Op.lesserThan)
  }

  def checkSumLE(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    Sum.checkBC(filteringTested, Op.lesserThanOrEqual)
  }

  def checkSumGT(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    Sum.checkBC(filteringTested, Op.greaterThan)
  }

  def checkSumGE(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    Sum.checkBC(filteringTested, Op.greaterThanOrEqual)
  }

  def checkElementAC(filteringTested: Array[Set[Int]] => Array[Set[Int]]): Unit = {
    Element.checkAC(filteringTested)
  }

  def checkTableAC(filteringWithTableTested: (Array[Set[Int]], Set[Array[Int]]) => Array[Set[Int]]): Unit = {
    Table.checkAC(filteringWithTableTested)
  }

  def checkGCC(filteringWithValuesTested: (Array[Set[Int]], Array[Int]) => Array[Set[Int]]): Unit = {
    Gcc.checkAC(filteringWithValuesTested)
  }
}