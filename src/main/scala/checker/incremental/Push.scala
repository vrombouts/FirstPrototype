package checker.incremental

/**
  * branching operation representing a push operation.
  * It means that the current state should be pushed
  * on the solver's trail.
  *
  * @param doms : 'should be' values of the domains
  *             before the push branching operation.
  */
class Push(val doms: Array[Set[Int]]) extends BranchOp(doms) {
  override def clone = new Push(domains.clone)

  override def toString = "Push"
}

