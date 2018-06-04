package checker

import Conversions._
import checker.incremental.BranchOp

/**
  * Abstract class representing an incremental
  * filtering algorithm thanks to its functions
  * 'setup' and 'branchAndFilter'.
  */
abstract class FilterWithState {
  /**
    * This function should instantiate a Constraint
    * Programming problem starting with the domains
    * contained in 'variables'. Once this function
    * ends, the problem should be accessible for a-
    * ll the functions of this class.
    *
    * @param variables : array of domains
    * @return filtered domains given the filtering
    *         algorithm it implements.
    */
  def setup(variables: Array[Set[Int]]): Array[Set[Int]]

  /**
    * This function should update the state of the
    * current problem in function of the branching
    * operation it must perform.
    *
    * @param branching : branching operation to e-
    *                  xecute. It can be a Pop, P-
    *                  ush or a RestrictDomain.
    * @return filtered domains after the applicat-
    *         ion of the branching operation and
    *         the following filtering according to
    *         the filtering algorithm it implements
    */
  def branchAndFilter(branching: BranchOp): Array[Set[Int]]
}

/**
  * Abstract class extending the FilterWithState class
  * to propose an alternative to the 'setup' and 'branchAndFilter'
  * functions for java developers with the 'setupJava'
  * and 'branchAndFilterJava' functions.
  */
abstract class JFilterWithState extends FilterWithState {
  override final def setup(variables: Array[Set[Int]]): Array[Set[Int]] = setupJava(variables)

  override final def branchAndFilter(branching: BranchOp): Array[Set[Int]] = branchAndFilterJava(branching)

  /**
    * This function should instantiate a Constraint
    * Programming problem starting with the domains
    * contained in 'variables'. Once this function
    * ends, the problem should be accessible for a-
    * ll the functions of this class.
    *
    * @param variables : array of domains
    * @return filtered domains given the filtering
    *         algorithm it implements.
    */
  def setupJava(variables: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

  /**
    * This function should update the state of the
    * current problem in function of the branching
    * operation it must perform.
    *
    * @param branching : branching operation to e-
    *                  xecute. It can be a Pop, P-
    *                  ush or a RestrictDomain.
    * @return filtered domains after the applicat-
    *         ion of the branching operation and
    *         the following filtering according to
    *         the filtering algorithm it implements
    */
  def branchAndFilterJava(branching: BranchOp): Array[java.util.Set[Integer]]
}
