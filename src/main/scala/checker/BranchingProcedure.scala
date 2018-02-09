package checker

class BranchingProcedure(operation: Int) {
  private var variable = 0

  def branch(variables: Array[Set[Int]]): List[BranchingConstraint] = {
    variable = variables.indexWhere(x => x.size>1)
    if (variable >= variables.length) return List()
    else branch(variables(variable))
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
    val bEqual = new BranchingConstraint(variable,domain.min,Op.equal)
    val bDifferent = new BranchingConstraint(variable,domain.min,Op.different)
    List(bEqual,bDifferent)
  }
  private def branchLesserThanOrEqual(domain: Set[Int]): List[BranchingConstraint] = {
    val middle = (domain.max+domain.min)/2
    val bLesserThanOrEqual = new BranchingConstraint(variable,middle,Op.lesserThanOrEqual)
    val bGreaterThan = new BranchingConstraint(variable,middle,Op.greaterThan)
    List(bLesserThanOrEqual,bGreaterThan)
  }
  private def branchValues(domain: Set[Int]): List[BranchingConstraint] = {
    domain.map(x => new BranchingConstraint(variable,x,Op.equal)).toList
  }
}

object Op {
  val equal = 0
  val different = 1
  val lesserThanOrEqual = 2
  val greaterThan = 3
  val values = 4
}
