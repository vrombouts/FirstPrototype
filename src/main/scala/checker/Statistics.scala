package checker

import java.io._

import checker.constraints.incremental.BranchOp

abstract class Statistics(nbBranchOp: Int, filename: String) {

  def this(filename: String) = this(25, filename)

  // stats about the number of executed tests
  private[this] var nbExecutedTests: Int = 0
  private[this] var nbNoSolutionTests: Int = 0
  private[this] var nbFailedNoSolutionTests: Int = 0


  private[this] var nbBacktracks: Int = 0
  private[this] var nbNodes: Int = 0
  private[this] var nbLeaves: Int = 0

  protected[this] val filenameStats: File = new File("out/statistics/" + filename + "/statistics.txt")
  filenameStats.getParentFile.mkdirs

  protected[this] val filenamePassed: File = new File("out/statistics/" + filename + "/passedTests.txt")
  filenamePassed.getParentFile.mkdirs

  protected[this] val filenameFailed: File = new File("out/statistics/" + filename + "/failedTests.txt")
  filenameFailed.getParentFile.mkdirs

  // stats about the generator
  protected[this] var generatorUsed: VariablesGenerator = _

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

  protected[this] var testsPassed: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  protected[this] var testsFailed: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  protected[this] var storedResults: Array[(BranchOp, Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  protected[this] var testsIncPassed: Array[(BranchOp, Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  protected[this] var testsIncFailed: Array[(BranchOp, Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

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
    val prWriterStats = new PrintWriter(filenameStats)
    val prWriterPassed = new PrintWriter(filenamePassed)
    val prWriterFailed = new PrintWriter(filenameFailed)
    printStats(isInc, prWriterStats)
    prWriterStats.close()
    printTests(prWriterPassed, testsPassed)
    printTests(prWriterFailed, testsFailed)
    printIncStats(prWriterPassed, testsIncPassed)
    printIncStats(prWriterFailed, testsIncFailed, passed = false)
    prWriterFailed.close()
    prWriterPassed.close()
  }

  private[this] def printStats(implicit isInc: Boolean = false, prWriter: PrintWriter): Unit = {
    prWriter.write(globalStatsToString(isInc))
    if (isInc)
      prWriter.write(branchingStatsToString())

    if (!(generatorUsed == null))
      prWriter.write(generatorUsed.toString)
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
      for (_ <- s.length until l)
        result += " "
    }
    result
  }

  private[this] def printTests(prWriter: PrintWriter, tests: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])], isInc: Boolean = false): Unit = {
    for ((init,ourDom,yourDom) <- tests) {
      var ourTitle: String = "Filtered domains "
      var yourTitle: String = "Your filtered domains "
      if (isInc) {
        ourTitle = "Correct domains "
        yourTitle = "Your domains "
      }
      var maxLength: Int = ourTitle.length
      init.foreach(t => if (t.size > maxLength) maxLength = domainToString(t).length)
      prWriter.write(extendString("Initial domains ", maxLength) + "|" + extendString(ourTitle, maxLength))
      if (yourDom != null)
        prWriter.write("|" + yourTitle)
      prWriter.write("\n")
      for (i <- init.indices) {
        prWriter.write(extendString(domainToString(init(i)), maxLength) + "|" + extendString(domainToString(ourDom(i)), maxLength))
        if (yourDom != null) prWriter.write("|" + domainToString(yourDom(i)))
        prWriter.write("\n")
      }
      prWriter.write("\n")
    }
  }

  private[this] def printOnALine(domains: Array[Set[Int]]): String = {
    var s: String = "["
    for (dom <- domains) {
      s += "["
      if (dom.nonEmpty) {
        s += dom.head
        for (elem <- dom.tail) {
          s += "," + elem
        }
      }
      s += "]"
    }
    s += "]"
    s
  }

  private[this] def printIncStats(prWriter: PrintWriter, tests: Array[(BranchOp, Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])], passed: Boolean = true): Unit = {
    //val prWriter = new PrintWriter(f)
    if (tests.isEmpty) {
      return
    }
    var lastTest: Array[Set[Int]] = null
    for (i <- tests.indices) {
      tests(i) match {
        case (b, d1, d2, null) =>
          if (lastTest == null) {
            if (!passed) prWriter.write("TEST FAILED : \n")
            else prWriter.write("TEST PASSED : \n")
            prWriter.write("Init : " + printOnALine(d1) + "\n")
          }
          lastTest = d2
          prWriter.write(b.toString + ": " + printOnALine(d2) + "\n")
        case (b, _, d2, d3) =>
          if (!passed) {
            prWriter.write(b.toString + " failed: \n" + "After the application of " + b.toString + "\n")
            printTests(prWriter, Array((lastTest, d2, d3)), isInc = true)
          }
          prWriter.write("------------------------------\n")
          lastTest = null
      }
    }
  }

  private[this] def allFixed(variables: Array[Set[Int]]): Boolean = {
    variables.forall(_.size == 1)
  }

  def strictDomainComparison(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]], init: Array[Set[Int]], result: Boolean): Unit

  def correctDomains(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]]): Boolean

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
      if (!correctDomains(ourReducedDomains, reducedDomains)) {
        if(b != null) println(b)
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
    if (result) {
      if (b == null)
        testsPassed = testsPassed :+ (init, ourReducedDomains, null)
      else {
        storedResults = storedResults :+ (b.head, init, ourReducedDomains, null)
      }
    }
    else {
      if (b == null)
        testsFailed = testsFailed :+ (init, ourReducedDomains, reducedDomains)
      else {
        storedResults = storedResults :+ (b.head, init, ourReducedDomains, reducedDomains)
        testsIncFailed = testsIncFailed ++ storedResults.clone()
        storedResults = Array()
      }
    }
    if (b != null && b.length == nbBranchOp) {
      storedResults = storedResults :+ (b.head, init, ourReducedDomains, reducedDomains)
      testsIncPassed = testsIncPassed ++ storedResults.clone()
      storedResults = Array()
    }
    result
  }

}
