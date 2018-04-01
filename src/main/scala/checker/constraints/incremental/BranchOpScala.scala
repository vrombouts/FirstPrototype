package checker.constraints.incremental

import java.util


class BranchOpScala(var domains: Array[util.Set[Integer]]) {
  override def clone = new BranchOp(domains.clone)

  override def toString = "No more branching possible"
}



