package checker

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}

import scala.collection.mutable

class ACFilteringIncremental(checker: Array[Int] => Boolean) extends FilterWithState {

  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  private[this] var domainsStorage: mutable.Stack[Array[Set[Int]]] = _

  private[this] val acFilter = new ACFiltering(checker)

  override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = {
    domainsStorage = mutable.Stack()
    acFilter.filter(variables)
    // why doing a push here??? setup should only make a static application of filtering
  }

  override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = {
    var restrictDomain: Array[Set[Int]] = Array()
    branching match {
      case _: Push => push(branching.domains)
      case _: Pop => pop(branching.domains)
      case restriction: RestrictDomain =>
        restrictDomain = restriction.applyRestriction
        acFilter.filter(restrictDomain)
      case _ => branching.domains
    }
  }

  def push(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    domainsStorage.push(currentDomain)
    currentDomain
  }

  def pop(currentDomain: Array[Set[Int]]): Array[Set[Int]] = {
    if (domainsStorage.nonEmpty)
      domainsStorage.pop()
    else
      currentDomain
  }

}
