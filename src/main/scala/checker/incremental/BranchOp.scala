package checker.incremental

class BranchOp(var domains: Array[Set[Int]]) {
  override def clone = new BranchOp(domains.clone)

  override def toString = "No more branching possible"
}



