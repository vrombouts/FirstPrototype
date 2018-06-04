package checker.incremental

/**
  * Each instance of this class represents a branching
  * operation performed from given 'domains'.
  *
  * @param domains : array of domains.
  */
class BranchOp(var domains: Array[Set[Int]]) {
  override def clone = new BranchOp(domains.clone)

  override def toString = "No more branching possible"
}



