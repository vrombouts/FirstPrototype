package checker

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker.constraints.Interval
import checker.constraints.incremental.{BranchOp, Pop, Push, RestrictDomain}

import scala.collection.mutable

class BCFilteringIncremental(checker: Array[Int] => Boolean) extends FilterWithState {
  def this(jChecker: Function[Array[Integer], java.lang.Boolean]) = this(checkerToScalaFunction(jChecker))

  private[this] var domainsStorage: mutable.Stack[Array[Set[Int]]] = _

  override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = {
    domainsStorage = mutable.Stack()
    filteringBC(variables)
  }

  override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = {
    var restrictDomain: Array[Set[Int]] = Array()
    branching match {
      case _: Push => push(branching.domains)
      case _: Pop => pop(branching.domains)
      case restriction: RestrictDomain =>
        restrictDomain = restriction.applyRestriction
        filteringBC(restrictDomain)
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

  //filtering BC
  //applying BC without pruning //
  @throws[NoSolutionException]
  def filteringBC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val intervals = getIntervals(variables)
    while (changeBounds(intervals)) {}
    intervalsToVariables(intervals)
  }

  @throws[NoSolutionException]
  private[this] def getIntervals(variables: Array[Set[Int]]): Array[Interval] = {
    variables.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
  }

  private[this] def intervalsToVariables(intervals: Array[Interval]): Array[Set[Int]] = {
    intervals.map(x => x.domain)
  }

  @throws[NoSolutionException]
  private[this] def changeBounds(intervals: Array[Interval]): Boolean = {
    var changed: Boolean = false
    for (i <- intervals.indices) {
      val modif: Boolean = applyBCOnOneValue(intervals, i)
      val other_modif: Boolean = applyBCOnOneValue(intervals, i, isMin = false)
      if (modif || other_modif) changed = true
    }
    changed
  }

  @throws[NoSolutionException]
  private[this] def applyBCOnOneValue(intervals: Array[Interval], index: Int, isMin: Boolean = true): Boolean = {
    if (findASolution(intervals, index, isMin))
      false
    else {
      if (intervals(index).domain.size == 1) throw new NoSolutionException
      intervals(index).update(isMin)
      true
    }
  }

  //Generation of all the solutions
  private[this] def findASolution(intervals: Array[Interval], index: Int, isMin: Boolean): Boolean = {
    val currentSol: Array[Int] = Array.fill(intervals.length)(0)
    currentSol(index) = intervals(index).giveValue(isMin)
    setIthVariable(intervals, 0, currentSol, index)
  }

  private[this] def setIthVariable(intervals: Array[Interval], currentIndex: Int, currentSol: Array[Int], index: Int): Boolean = {
    val range = if (currentIndex == index) 0 until 1 else intervals(currentIndex).getRange
    for (i <- range) {
      if (currentIndex != index)
        currentSol(currentIndex) = i
      if (currentIndex == intervals.length - 1) {
        if (checker(currentSol)) {
          return true

        }
      } else {
        if (setIthVariable(intervals, currentIndex + 1, currentSol, index))
          return true
      }
    }
    false
  }

}
