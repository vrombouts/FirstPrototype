package checker

class BranchingConstraint(private val variable:Int,private val constant: Int,private val operation: Int) {
  def this(variable: Int, operation:Int) = this(variable,0,operation) //for constraint without constants
  def this(operation:Int) = this(0,operation) //for global constraint
  def this() = this(0,0,-1) //for True constraint

  def applyOn(variables: Array[Set[Int]]): Unit = {
    variables(variable) = applyOnDomain(variables(variable))
  }

  def applyOnDomain(domain: Set[Int]): Set[Int] = {
    operation match {
      case Op.equal             =>  applyEqual(domain)
      case Op.different         =>  applyDifferent(domain)
      case Op.lesserThanOrEqual =>  applyLesserThanOrEqual(domain)
      case Op.greaterThan       =>  applyGreaterThan(domain)
      case _ => domain //default case is True constraint
    }
  }
  private def applyEqual(domain: Set[Int]): Set[Int] = domain.filter(x => x==constant)
  private def applyLesserThanOrEqual(domain: Set[Int]): Set[Int] = domain.filter(x => x<=constant)
  private def applyDifferent(domain: Set[Int]): Set[Int] = domain.filter(x => x!=constant)
  private def applyGreaterThan(domain: Set[Int]): Set[Int] = domain.filter(x => x>constant)
}
