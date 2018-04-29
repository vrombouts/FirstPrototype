package checker

trait Filterings extends Base {

  def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    propagation match {
      case `AC` => applyConstraintAC(variables)
      case `BC` => applyConstraintBC(variables)
      case _ => applyConstraintAC(variables)
    }
  }

  protected[this] def applyConstraintAC(variables: Array[Set[Int]]): Array[Set[Int]]

  protected[this] def applyConstraintBC(variables: Array[Set[Int]]): Array[Set[Int]]
}
