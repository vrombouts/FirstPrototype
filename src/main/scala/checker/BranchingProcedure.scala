package checker

class BranchingProcedure(variable: Int, constant: Int, operation: Int) {

  def branch(variables: Array[Set[Int]]): List[BranchingConstraint] = {
    branch(variables(variable))
  }

  def branch(domain: Set[Int]) : List[BranchingConstraint] ={
    operation match{
      case Op.equal             => branchEqual(domain)
      case Op.lesserThanOrEqual => branchLesserThanOrEqual(domain)
      case Op.values            => branchValues(domain)
      case _ => List()
    }

  }
  private def branchEqual(domain : Set[Int]): List[BranchingConstraint] = {
    val bEqual = new BranchingConstraint(variable,constant,Op.equal)
    val bDifferent = new BranchingConstraint(variable,constant,Op.different)
    List(bEqual,bDifferent)
  }
  private def branchLesserThanOrEqual(domain: Set[Int]): List[BranchingConstraint] = {
    val bLesserThanOrEqual = new BranchingConstraint(variable,constant,Op.lesserThanOrEqual)
    val bGreaterThan = new BranchingConstraint(variable,constant,Op.greaterThan)
    List(bLesserThanOrEqual,bGreaterThan)
  }
  private def branchValues(domain: Set[Int]): List[BranchingConstraint] = {
    domain.map(x => new BranchingConstraint(variable,x,Op.equal)).toList
  }
}

object Op {
  def equal = 0
  def different = 1
  def lesserThanOrEqual = 2
  def greaterThan = 3
  def values = 4
}
