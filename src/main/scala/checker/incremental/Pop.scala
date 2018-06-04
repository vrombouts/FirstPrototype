package checker.incremental

/**
  * branching operation representing a pop operation.
  * It means that the trail of the solver should be
  * popped.
  *
  * @param doms : 'should be' values of the domains
  *             before the pop branching operation.
  */
class Pop(val doms: Array[Set[Int]]) extends BranchOp(doms) {
  override def clone = new Pop(domains.clone)

  override def toString = "Pop"
}