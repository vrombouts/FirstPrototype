package checker

import java.util.function.Function
import Conversions._
import checker.constraints.incremental.BranchOp
import checker.constraints._

class JCpChecker(c: Checks) {
  def this() = this(new Constraint)

  def gen(): VariablesGenerator = c.gen

  def checkAC(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    c.checkAC(filtering, checker)
  }

  def checkBC(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    c.checkBC(filtering, checker)
  }

  def check(filtering: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
            checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    c.check(filtering, checker)
  }

  def checkAC(init: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              filtering: Function[BranchOp, Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    c.checkAC(init, filtering, checker)
  }

  def checkBC(init: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              filtering: Function[BranchOp, Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    c.checkBC(init, filtering, checker)
  }

  def check(init: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]],
              filtering: Function[BranchOp, Array[java.util.Set[Integer]]],
              checker: Function[Array[Integer], java.lang.Boolean]): Unit = {
    c.check(init, filtering, checker)
  }


}