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
  protected[this] var generatorUsed: VariablesGenerator = _

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

  def getNbLeaves: Int = nbLeaves

  def getGenerator: VariablesGenerator = generatorUsed

  def nbFailedTests: Int


  def setGenerator(gen: VariablesGenerator): Unit = generatorUsed = gen

  def globalStatsToString(isInc: Boolean): String

  var testsPassed: Array[(Array[Set[Int]], Array[Set[Int]])] = Array()

  var testsFailed: Array[(Array[Set[Int]], Array[Set[Int]])] = Array()


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

  def print(implicit isInc: Boolean = false): Unit = {
    printStats(isInc)
    printTests("passedTests.txt", testsPassed)
    printTests("failedTests.txt", testsFailed)
  }

  private[this] def printStats(implicit isInc: Boolean = false): Unit = {
    val f: File = new File("out/statistics.txt")
    f.getParentFile.mkdirs
    val prWriter = new PrintWriter(f)
    prWriter.write(globalStatsToString(isInc))
    if (isInc)
      prWriter.write(branchingStatsToString())

    if (!(generatorUsed == null))
      prWriter.write(generatorUsed.toString)
    prWriter.close()
  }

  def domainToString(dom: Set[Int]): String = {
    var result: String = "["
    var first: Boolean = true
    for (elem <- dom) {
      if (first) {
        result += elem
        first = false
      }
      else result += ", " + elem
    }
    result += "]"
    result
  }

  private[this] def extendString(s: String, l: Int): String = {
    var result: String = s
    if (s.length != l) {
      for (i <- s.length until l)
        result += " "
    }
    result
  }

  private[this] def printTests(filename: String, tests: Array[(Array[Set[Int]], Array[Set[Int]])]): Unit = {
    val f: File = new File("out/" + filename)
    f.getParentFile.mkdirs
    val prWriter = new PrintWriter(f)
    for (test <- tests) {
      var maxLength: Int = "Initial domains ".length
      test._1.foreach(t => if (t.size > maxLength) maxLength = domainToString(t).length)
      prWriter.write(extendString("Initial domains ", maxLength) + "|" + "Filtered domains \n")
      (test._1 zip test._2).foreach(x => prWriter.write(extendString(domainToString(x._1), maxLength) + "|" + domainToString(x._2) + "\n"))
      prWriter.write("\n")
    }
    prWriter.close()
  }

  private[this] def allFixed(variables: Array[Set[Int]]): Boolean = {
    variables.forall(_.size == 1)
  }

  def strictDomainComparison(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]], init: Array[Set[Int]], result: Boolean): Unit

  def incorrectDomains(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]]): Boolean

  private[this] def printer(returnValues: Array[Array[Set[Int]]]): Unit = {
    val initial: Array[Set[Int]] = returnValues(0)
    val reduced: Array[Set[Int]] = returnValues(1)
    val trueReduced: Array[Set[Int]] = returnValues(2)
    if (reduced.exists(x => x.isEmpty) && trueReduced.forall(x => x.nonEmpty)) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you claim there is no solution")
    } else if (reduced.forall(x => x.nonEmpty) && trueReduced.exists(x => x.isEmpty)) {
      println("failed for: " + initial.toList)
      println("you should not have any solutions")
      println("but you had: " + reduced.toList)
    } else if (reduced.forall(x => x.nonEmpty) && trueReduced.forall(x => x.nonEmpty)) {
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
    else if (ourReducedDomains.forall(_.nonEmpty) && reducedDomains.exists(_.isEmpty)) {
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
    incNbExecutedTests()
    if (ourReducedDomains.exists(_.isEmpty)) {
      incNbNoSolutionTests()
      if (!result) incNbFailedNoSolutionTests()
    }
    else
      strictDomainComparison(ourReducedDomains, reducedDomains, init, result)
    if (result) testsPassed = testsPassed :+ (init, ourReducedDomains)
    else testsFailed = testsFailed :+ (init, ourReducedDomains)
    result
  }

}
