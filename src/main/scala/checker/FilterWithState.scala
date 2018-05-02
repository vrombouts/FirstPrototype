package checker

import checker.constraints.incremental.BranchOp

abstract class FilterWithState {
  def setup(variables: Array[Set[Int]]): Array[Set[Int]]
  def branchAndFilter(branching: BranchOp): Array[Set[Int]]
}
