package checker

import checker.constraints.incremental.BranchOp
import Conversions._

abstract class FilterWithState {
  def setup(variables: Array[Set[Int]]): Array[Set[Int]]

  def branchAndFilter(branching: BranchOp): Array[Set[Int]]
}

abstract class JFilterWithState extends FilterWithState {
  override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = setupJava(variables)

  override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = branchAndFilterJava(branching)

  def setupJava(variables: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

  def branchAndFilterJava(branching: BranchOp): Array[java.util.Set[Integer]]
}
