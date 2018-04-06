package checker

import java.io._

import checker.constraints.incremental.BranchOp

abstract class Statistics {

  // stats about the number of executed tests
  private[this] var nbExecutedTests: Int = 0
  private[this] var nbNoSolutionTests: Int = 0
  private[this] var nbFailedNoSolutionTests: Int = 0


  private[this] var nbBacktracks: Int = 0
  private[this] var nbNodes: Int = 0
  private[this] var nbLeaves: Int = 0

  // stats about the generator
  private[this] var generatorUsed: VariablesGenerator = _

  //private[this] var lastTestFail:(Array[Set[Int]],Array[Set[Int]],Array[Set[Int]])=(null,null,null)

  def incNbExecutedTests(): Unit = nbExecutedTests += 1

  def incNbNoSolutionTests(): Unit = nbNoSolutionTests += 1

  def incNbFailedNoSolutionTests(): Unit = nbFailedNoSolutionTests += 1

  def incNbBacktracks(): Unit = nbBacktracks += 1

  def incNbNodes(): Unit = nbNodes += 1

  def incNbLeaves(): Unit = nbLeaves += 1

  def getNbExecutedTests: Int = nbExecutedTests

  def getNbNoSolutionTests: Int = nbNoSolutionTests

  def getNbFailedNoSolutionTests: Int = nbFailedNoSolutionTests

  def nbFailedTests: Int


  def setGenerator(gen: VariablesGenerator): Unit = generatorUsed = gen

  def globalStatsToString(): String


  def printNumber(nb: Int): String = {
    val nbOfChars: Int = nb.toString.length
    var s: String = " " + nb
    for (_ <- 1 to 10 - nbOfChars) {
      s = s + " "
    }
    s
  }


  def branchingStatsToString(): String = {
    "The average number of backtracks per test is " + nbBacktracks / nbExecutedTests + "\n" +
      "The average number of nodes per test is " + nbNodes / nbExecutedTests + "\n" +
      "The average number of leaves per test is " + nbLeaves / nbExecutedTests + "\n"
  }

  def printStats(implicit isInc: Boolean = false): Unit = {
    val f: File = new File("out/statistics.txt")
    f.getParentFile.mkdirs
    val prWriter = new PrintWriter(f)
    prWriter.write(globalStatsToString())
    if (isInc) {
      prWriter.write(branchingStatsToString())
    }

    if (!(generatorUsed == null))
      prWriter.write(generatorUsed.toString)
    prWriter.close()
  }

  private[this] def allFixed(variables: Array[Set[Int]]): Boolean = {
    variables.forall(_.size == 1)
  }

  def strictDomainComparison(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]], init: Array[Set[Int]], result: Boolean): Unit

  def incorrectDomains(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]]): Boolean

  protected def printer(returnValues: Array[Array[Set[Int]]]): Unit = {
    val initial: Array[Set[Int]] = returnValues(0)
    val reduced: Array[Set[Int]] = returnValues(1)
    val trueReduced: Array[Set[Int]] = returnValues(2)
    if (reduced.isEmpty && trueReduced.nonEmpty) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you claim there is no solution")
    } else if (reduced.nonEmpty && trueReduced.isEmpty) {
      println("failed for: " + initial.toList)
      println("you should not have any solutions")
      println("but you had: " + reduced.toList)
    } else if (reduced.nonEmpty && trueReduced.nonEmpty) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you had: " + reduced.toList)
    }
  }

  /*
   * returns true if the domains that have been reduced by our function are the same that the domains being reduced by the user function
   */
  def comparison(returnValues: Array[Array[Set[Int]]], b: List[BranchOp] = null): Boolean = {
    val ourReducedDomains: Array[Set[Int]] = returnValues(2)
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val init: Array[Set[Int]] = returnValues(0)
    var result: Boolean = true
    if (reducedDomains == null) {
      println("You returned a null array instead of an array of filtered domains")
      result = false
    }
    else if (ourReducedDomains.length != reducedDomains.length) {
      println("Incorrect output format : you don't return the correct number of domains variables")
      result = false
    }
    else if (ourReducedDomains.exists(_.isEmpty) && reducedDomains.exists(_.isEmpty)) {
      if (b != null)
        incNbLeaves()
      result = true
    }
    else if ((ourReducedDomains.exists(_.isEmpty) && reducedDomains.forall(_.nonEmpty)) ||
      (ourReducedDomains.forall(_.nonEmpty) && reducedDomains.exists(_.isEmpty))) {
      printer(returnValues)
      result = false
    }
    else {
      if (b != null && allFixed(reducedDomains))
        incNbLeaves()
      if (incorrectDomains(ourReducedDomains, reducedDomains)) {
        printer(returnValues)
        result = false
      }
    }
    if (b != null && !result) println("with all those branches in reverse order: " + b)
    if (b == null) {
      incNbExecutedTests()
      if (ourReducedDomains.exists(_.isEmpty)) {
        incNbNoSolutionTests()
        if (!result) incNbFailedNoSolutionTests()
      }
      else {
        strictDomainComparison(ourReducedDomains, reducedDomains, init, result)
      }

    }
    result
  }

}
