package checker.statistics

import java.io._

import checker.TestArgs
import checker.incremental.{BranchOp, Pop, Push}

abstract class Statistics(var filename: String) {

  // stats about the number of executed tests
  private[this] var nbExecutedTests: Int = 0
  private[this] var nbNoSolutionTests: Int = 0
  private[this] var nbFailedNoSolutionTests: Int = 0

  protected[this] var filenameStats: File = new File("out/statistics/" + filename + "/statistics.txt")
  filenameStats.getParentFile.mkdirs

  protected[this] var filenamePassed: File = new File("out/statistics/" + filename + "/passedTests.txt")
  filenamePassed.getParentFile.mkdirs

  protected[this] var filenameFailed: File = new File("out/statistics/" + filename + "/failedTests.txt")
  filenameFailed.getParentFile.mkdirs

  // stats about the generator
  protected[this] var generatorUsed: TestArgs = _

  protected[this] var nbPush: Int = 0
  protected[this] var nbPop: Int = 0
  protected[this] var nbRestriction: Int = 0

  private[this] var nbTestCases: Int = 0

  def setFileName(filename: String): Unit = {
    filenameStats = new File("out/statistics/" + filename + "/statistics.txt")
    filenameStats.getParentFile.mkdirs
    filenamePassed = new File("out/statistics/" + filename + "/passedTests.txt")
    filenamePassed.getParentFile.mkdirs
    filenameFailed = new File("out/statistics/" + filename + "/failedTests.txt")
    filenameFailed.getParentFile.mkdirs
    this.filename = filename
  }

  protected[this] def incNbExecutedTests(): Unit = nbExecutedTests += 1

  protected[this] def incNbNoSolutionTests(): Unit = nbNoSolutionTests += 1

  protected[this] def incNbFailedNoSolutionTests(): Unit = nbFailedNoSolutionTests += 1

  protected[this] def getNbExecutedTests: Int = nbExecutedTests

  protected[this] def getNbNoSolutionTests: Int = nbNoSolutionTests

  protected[this] def getNbFailedNoSolutionTests: Int = nbFailedNoSolutionTests

  def getGenerator: TestArgs = generatorUsed

  protected[this] def nbFailedTests: Int

  def setGenerator(gen: TestArgs): Unit = generatorUsed = gen

  /*
   * This function prints the computed statistics in a table
   */
  protected[this] def globalStatsToString(isInc: Boolean): String

  /*
   * This function updates the stats internal variables according to the type of stats (child inheritance)
   */
  protected[this] def updateStats(bugFreeReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]], init: Array[Set[Int]], result: Boolean): Unit

  /*
   * This function makes the comparison of the domains passed in argument.
   */
  protected[this] def correctDomains(bugFreeReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]]): Boolean


  protected[this] var testsPassed: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  protected[this] var testsFailed: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  protected[this] var storedResults: Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])] = Array()

  protected[this] var testsIncPassed: Array[Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])]] = Array()

  protected[this] var testsIncFailed: Array[Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])]] = Array()

  private[this] def updateBranching(b: List[BranchOp]): Unit = {
    if (b != null && b.nonEmpty) {
      b.head match {
        case _: Push => nbPush += 1
        case _: Pop => nbPop += 1
        case _ => nbRestriction += 1
      }
    }
  }

  protected[this] def printNumber(nb: Int): String = {
    val nbOfChars: Int = nb.toString.length
    var s: String = " " + nb
    for (_ <- 1 to 10 - nbOfChars) {
      s = s + " "
    }
    s
  }


  private[this] def branchingStatsToString(): String = {
    var nbPushOp: Int = 0
    var nbPopOp: Int = 0
    var nbRestrictOp: Int = 0
    if (nbPush != 0) nbPushOp = nbPush / nbTestCases
    if (nbPop != 0) nbPopOp = nbPop / nbTestCases
    if (nbRestriction != 0) nbRestrictOp = nbRestriction / nbTestCases
    "The average number of Push operation per test is " + nbPushOp + "\n" +
      "The average number of Pop operation per test is " + nbPopOp + "\n" +
      "The average number of Restrict Domain operation per test is " + nbRestrictOp + "\n"
  }

  def print(implicit isInc: Boolean = false): Unit = {
    val prWriterStats = new PrintWriter(filenameStats)
    val prWriterPassed = new PrintWriter(filenamePassed)
    val prWriterFailed = new PrintWriter(filenameFailed)
    printStats(isInc, prWriterStats)
    prWriterStats.close()
    printTests(prWriterPassed, testsPassed)
    printTests(prWriterFailed, testsFailed, passed = false)
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

  private[this] def domainToString(dom: Set[Int]): String = {
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

  private[this] def printTests(prWriter: PrintWriter, tests: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])], isInc: Boolean = false, passed: Boolean = true): Unit = {
    for ((init, bugFreeDom, testedDom) <- tests) {
      var bugFreeTitle: String = "Correct filtering "
      var testedTitle: String = "Tested filtering "
      if (isInc) {
        bugFreeTitle = "Bugfree domains "
        testedTitle = "Tested domains "
      }
      var maxLength: Int = bugFreeTitle.length
      init.foreach(t => if (t.size > maxLength) maxLength = domainToString(t).length)
      var tested: Array[Set[Int]] = testedDom.clone()
      if (tested != null && testedDom.length < init.length)
        tested = tested ++ Array.fill(init.length - tested.length)(null)
      prWriter.write(extendString("Initial domains ", maxLength) + "|" + extendString(bugFreeTitle, maxLength))
      prWriter.write("|" + testedTitle)
      prWriter.write("\n")
      for (i <- init.indices) {
        prWriter.write(extendString(domainToString(init(i)), maxLength) + "|" + extendString(domainToString(bugFreeDom(i)), maxLength))
        if (tested != null && tested(i) != null) prWriter.write("|" + domainToString(tested(i)))
        else prWriter.write("|" + "null")
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

  private[this] def printIncStats(prWriter: PrintWriter, tests: Array[Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])]], passed: Boolean = true): Unit = {
    //val prWriter = new PrintWriter(f)
    if (tests.isEmpty) {
      return
    }
    var lastTest: Array[Set[Int]] = null
    for (i <- tests.indices) {
      if (!tests(i).isEmpty) {
        if (!passed) prWriter.write("TEST FAILED : \n")
        else prWriter.write("TEST PASSED : \n")
        val (_, initial, _) = tests(i)(0)
        prWriter.write("Init : " + printOnALine(initial) + "\n")
        for (j <- 0 until tests(i).length - 1) {
          val (b, _, curr) = tests(i)(j)
          prWriter.write(b.toString + ": " + printOnALine(curr) + "\n")
          lastTest = curr
        }
        val (b, unk, curr) = tests(i)(tests(i).length - 1)
        if (!passed) {
          // in this case, unk refers to reducedDomains
          prWriter.write(b.toString + " failed: \n" + "After the application of " + b.toString + ": \n")
          printTests(prWriter, Array((lastTest, curr, unk)), isInc = true)
        }
        prWriter.write("------------------------------\n")
      }
    }
    if (storedResults.nonEmpty) {
      testsIncPassed = testsIncPassed :+ storedResults.clone()
      storedResults = Array()
    }
  }

  private[this] def updateGeneralStats(bugFreeReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]], init: Array[Set[Int]], b: List[BranchOp], result: Boolean): Unit = {
    incNbExecutedTests()
    if (b != null && b.size == 1) {
      nbTestCases += 1
      testsIncPassed = testsIncPassed :+ storedResults.clone()
      storedResults = Array()
    }
    if (bugFreeReducedDomains.exists(_.isEmpty)) {
      incNbNoSolutionTests()
      if (!result) incNbFailedNoSolutionTests()
    }
    else
      updateStats(bugFreeReducedDomains, reducedDomains, init, result)
    if (result) {
      if (b == null)
        testsPassed = testsPassed :+ (init, bugFreeReducedDomains, reducedDomains)
      else {
        storedResults = storedResults :+ (b.head, init, bugFreeReducedDomains)
      }
    }
    else {
      if (b == null)
        testsFailed = testsFailed :+ (init, bugFreeReducedDomains, reducedDomains)
      else {
        storedResults = storedResults :+ (b.head, reducedDomains, bugFreeReducedDomains)
        testsIncFailed = testsIncFailed :+ storedResults.clone()
        storedResults = Array()
      }
    }
    updateBranching(b)
  }


  /*
   * returns true if the domains that have been reduced by our function are the same that the domains being reduced by the user function
   */
  def comparison(returnValues: Array[Array[Set[Int]]], b: List[BranchOp] = null): Boolean = {
    var errorMsg: String = ""
    val bugFreeReducedDomains: Array[Set[Int]] = returnValues(2)
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val init: Array[Set[Int]] = returnValues(0)
    if (reducedDomains == null)
      errorMsg = "You returned a null array instead of an array of filtered domains"
    else if (bugFreeReducedDomains.length != reducedDomains.length)
      errorMsg = "Incorrect output format : you don't return the correct number of domains variables"
    else if (bugFreeReducedDomains.exists(_.isEmpty) && reducedDomains.exists(_.isEmpty))
      errorMsg = ""
    else if (bugFreeReducedDomains.forall(_.nonEmpty) && reducedDomains.exists(_.isEmpty))
      errorMsg = "The tested filtering removes solutions (filters too much domains)"
    else {
      if (!correctDomains(bugFreeReducedDomains, reducedDomains))
        errorMsg = "Some domains are not correctly filtered. Look at out/statistics/" + filename + "failedTests.txt"
    }
    updateGeneralStats(bugFreeReducedDomains, reducedDomains, init, b, errorMsg.isEmpty)
    if (errorMsg.nonEmpty) println(errorMsg)
    errorMsg.isEmpty
  }
}
