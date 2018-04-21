package checker.constraints

import checker.Base

trait Static extends Base{
  def checkConstraint(variables: Array[Set[Int]],
                      constraintTested: Array[Set[Int]] => Array[Set[Int]])
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    val returnValues: Array[Array[Set[Int]]] = Array(variables,
      apply(variables.length, variables.clone(), constraintTested),
      apply(variables.length, variables.clone(), applyConstraint(_: Array[Set[Int]])))
    //Then, we compare the two. If they are not equals, the constraint is not correct.
    stats.comparison(returnValues)
  }
}
