package checker

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}

//to do new IncrementalFiltering(myStaticFilter) and have a filterWithState from a static filter.
class IncrementalFiltering(filter: Filter) extends FilterWithState {
  //use list instead of stack.
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

  def push(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    trailStack = currentDomain :: trailStack
    currentDomain
  }

  def pop(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    if (trailStack.nonEmpty) {
      val vars = trailStack.head
      trailStack = trailStack.tail
      vars
    } else
      currentDomain
  }
}
