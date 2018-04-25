package checker.constraints.incremental

class Push(val doms: Array[Set[Int]]) extends BranchOp(doms) {
  override def clone = new Push(domains.clone)

  override def toString = "Push"
}

