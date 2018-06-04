package checker.incremental

import checker.{Filter, FilterWithState}

/**
  * This class implements the FilterWithState abstract class
  * to represent a static filtering algorithm as an increme-
  * ntal filtering algorithm. The filtering algorithm is de-
  * fined by the 'filter' object given to its constructor.
  *
  * @param filter : a Filter object representing the static
  *               filtering algorithm
  */
class IncrementalFiltering(filter: Filter) extends FilterWithState {
  //use a list which behaves as a stack for the trailing mechanisms
  private[this] var trailStack: List[Array[Set[Int]]] = _

  override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = {
    trailStack = List()
    filter.filter(variables)
  }

  override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = {
    var restrictDomain: Array[Set[Int]] = Array()
    branching match {
      case _: Push => push(branching.domains)
      case _: Pop => pop(branching.domains)
      case restriction: RestrictDomain =>
        restrictDomain = restriction.applyRestriction
        filter.filter(restrictDomain)
      case _ => branching.domains
    }
  }

  //push operation of the trail
  def push(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    trailStack = currentDomain :: trailStack
    currentDomain
  }

  //pop operation of the trail
  def pop(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    if (trailStack.nonEmpty) {
      val vars = trailStack.head
      trailStack = trailStack.tail
      vars
    } else
      currentDomain
  }
}
