package checker.incremental

class Pop(val doms: Array[Set[Int]]) extends BranchOp(doms) {
  override def clone = new Pop(domains.clone)

  override def toString = "Pop"
}