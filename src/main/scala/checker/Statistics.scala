package checker

import java.io._

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}

class Statistics(var folderName: String) {

  // stats about the number of executed tests
  private[this] var nbExecutedTests: Int = 0
  private[this] var nbFailedTests: Int = 0

  def getNbFailedTests: Int = nbFailedTests

  def getAlgo1Equals2: Int = algo1Equals2

  def getAlgo1FilterMoreThan2: Int = algo1FilterMoreThan2

  def getAlgo2FilterMoreThan1: Int = algo2FilterMoreThan1

  def getAlgo1FilterDiffThan2: Int = algo1FilterDiffThan2

  def getAlgo1FilterNoValue: Int = algo1FilterNoVal

  def getAlgo1FoundsInstantiation: Int = algo1FoundsInstantiation

  def getAlgo2FoundsInstantiation: Int = algo2FoundsInstantiation

  def getAlgo1FoundsNoSol: Int = algo1FoundsNoSol

  def getAlgo2FoundsNoSol: Int = algo2FoundsNoSol

  def getErrorMsg: String = errorMessage.toString()

  private[this] var algo1Equals2: Int = 0
  private[this] var algo1FilterMoreThan2: Int = 0
  private[this] var algo2FilterMoreThan1: Int = 0
  private[this] var algo1FilterDiffThan2: Int = 0
  private[this] var algo1FoundsInstantiation: Int = 0
  private[this] var algo2FoundsInstantiation: Int = 0
  private[this] var algo1FoundsNoSol: Int = 0
  private[this] var algo2FoundsNoSol: Int = 0
  private[this] var algo1FilterNoVal: Int = 0

  private[this] var errorMessage = new StringBuilder

  private[this] var lastFailed: String = ""

  private[this] var firstFailed: String = ""

  private[this] var filenameStats: File = _

  private[this] var filenamePassed: File = _

  private[this] var filenameFailed: File = _

  // stats about the generator
  private[this] var generatorUsed: TestArgs = _

  private[this] var depth: Int = 0
  private[this] var lastPop: Boolean = false
  private[this] var depths: List[Int] = List()

  private[this] var nbTestCases: Int = 0

  def setFolderName(filename: String): Unit = {
    filenameStats = new File("out/statistics/" + filename + "/statistics.txt")
    filenameStats.getParentFile.mkdirs
    filenamePassed = new File("out/statistics/" + filename + "/passedTests.txt")
    filenamePassed.getParentFile.mkdirs
    filenameFailed = new File("out/statistics/" + filename + "/failedTests.txt")
    filenameFailed.getParentFile.mkdirs
    this.folderName = filename
  }

  def getNbExecutedTests: Int = nbExecutedTests

  def getGenerator: TestArgs = generatorUsed

  def setGenerator(gen: TestArgs): Unit = generatorUsed = gen

  /*
   * This function prints the computed statistics in a table
   */
  private[this] def globalStatsToString(isInc: Boolean): String = {
    val nbTestEx: String = "Number of executed tests : "
    val nbFailedEx: String = "Number of failed tests : "
    val algo1EqualsAlgo2: String = "Tested and trusted algorithms filters the same : "
    val algo1FiltersMore: String = "Tested algorithm filters less than the trusted : "
    val algo2FiltersMore: String = "Tested algorithm filters more than the trusted (stronger) : "
    val algo1DiffThan2: String = "Trusted algorithm and tested one filters differently \n(without one stronger than the other) : "
    val algo1FiltersNoVal: String = "Trusted algorithm filters no value : "
    val ratioAlgo12ForNoSol: String = "Ratio of filtering until no solution found (tested algo/ trusted algo) : "
    val ratioAlgo12ForInstan: String = "Ratio of filtering until a single solution (tested algo/ trusted algo) : "

    "Here are the statistics of the executed tests :  \n" +
      "-------------------------------------------- \n \n" +
      nbTestEx + nbExecutedTests + "\n" +
      nbFailedEx + nbFailedTests + "\n\n" +
      algo1EqualsAlgo2 + algo1Equals2 + "\n" +
      algo1FiltersMore + algo1FilterMoreThan2 + "\n" +
      algo2FiltersMore + algo2FilterMoreThan1 + "\n" +
      algo1DiffThan2 + algo1FilterDiffThan2 + "\n\n" +
      algo1FiltersNoVal + algo1FilterNoVal + "\n\n" +
      ratioAlgo12ForNoSol + algo2FoundsNoSol + "/" + algo1FoundsNoSol + "\n" +
      ratioAlgo12ForInstan + algo2FoundsInstantiation + "/" + algo1FoundsInstantiation + "\n\n"

  }

  private[this] var testsPassed: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  private[this] var testsFailed: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])] = Array()

  private[this] var storedResults: Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])] = Array()

  private[this] var testsIncPassed: Array[Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])]] = Array()

  private[this] var testsIncFailed: Array[Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])]] = Array()

  private[this] def updateBranching(b: List[BranchOp]): Unit = {
    if (b != null && b.nonEmpty) {
      b.head match {
        case _: Push => lastPop = false
        case _: Pop =>
          if (lastPop) depth -= 1
          else {
            lastPop = true
            depths = depth :: depths
            depth -= 1
          }
        case _: RestrictDomain => depth += 1
      }
    }
    else {
      depth = 0
      lastPop = false
    }
  }


  private[this] def branchingStatsToString(): String = {
    var maxDepth: Int = 0
    var avgDepth: Int = 0
    if (depths.nonEmpty) {
      avgDepth = depths.sum / depths.length
      maxDepth = depths.max
    }
    depths = List()
    depth = 0
    "The average depth reached by the dives is " + avgDepth + "\n" +
      "The maximum depth reached is " + maxDepth + "\n\n"
  }

  def print(implicit isInc: Boolean = false): Unit = {
    setFolderName(folderName)
    val prWriterStats = new PrintWriter(filenameStats)
    val prWriterPassed = new PrintWriter(filenamePassed)
    val prWriterFailed = new PrintWriter(filenameFailed)
    printStats(isInc, prWriterStats)
    prWriterStats.close()
    printTests(prWriterPassed, testsPassed, isInc = isInc)
    errorMessage = printTests(prWriterFailed, testsFailed, passed = false, isInc = isInc)
    // store the last test
    if (storedResults.nonEmpty) {
      testsIncPassed = testsIncPassed :+ storedResults.clone()
      storedResults = Array()
    }
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

  private[this] def printTests(prWriter: PrintWriter, tests: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])], isInc: Boolean = false, passed: Boolean = true): StringBuilder = {
    val errMsg: StringBuilder = new StringBuilder
    for (i <- tests.length - 1 to 0 by -1) {
      val (init, bugFreeDom, testedDom) = tests(i)
      var bugFreeTitle: String = "Trusted filtering "
      var testedTitle: String = "Tested filtering "
      if (isInc) {
        bugFreeTitle = "Trusted domains "
        testedTitle = "Tested domains "
      }
      var maxLength: Int = bugFreeTitle.length
      init.foreach(t => if (t.size > maxLength) maxLength = domainToString(t).length)
      var tested: Array[Set[Int]] = testedDom.clone()
      if (tested != null && testedDom.length < init.length)
        tested = tested ++ Array.fill(init.length - tested.length)(null)
      var str: StringBuilder = new StringBuilder
      str ++= extendString("Initial domains ", maxLength) + "|" + extendString(bugFreeTitle, maxLength)
      str ++= "|" + testedTitle
      str ++= "\n"
      for (i <- init.indices) {
        str ++= extendString(domainToString(init(i)), maxLength) + "|" + extendString(domainToString(bugFreeDom(i)), maxLength)
        if (tested != null && tested(i) != null) str ++= "|" + domainToString(tested(i))
        else str ++= "|" + "null"
        str ++= "\n"
      }
      str ++= "\n"
      if(!isInc) prWriter.write(str.toString())
      if (!passed) {
        if (i == tests.length - 1) errMsg ++= "TEST FAILED FOR : \n" + str
        else if (i == 0) errMsg ++= "ORIGINAL TEST FAILED : \n" + str
      }
    }
    errMsg
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
    if (tests.isEmpty) {
      return
    }
    var lastTest: Array[Set[Int]] = null
    for (i <- tests.length - 1 to 0 by -1) {
      if (!tests(i).isEmpty) {
        if (!passed) prWriter.write("TEST FAILED : \n")
        else prWriter.write("TEST PASSED : \n")
        val (_, initial, _) = tests(i)(0)
        var str: StringBuilder = new StringBuilder
        str ++= "Init : " + printOnALine(initial) + "\n"
        for (j <- 0 until tests(i).length - 1) {
          val (b, _, curr) = tests(i)(j)
          str ++= b.toString + ": " + printOnALine(curr) + "\n"
          lastTest = curr
        }
        val (b, unk, curr) = tests(i)(tests(i).length - 1)
        if (!passed) {
          // in this case, unk refers to reducedDomains
          str ++= b.toString + " failed: \n" + "After the application of " + b.toString + ": \n"
          str ++= printTests(prWriter, Array((lastTest, curr, unk)), isInc = true, passed = false)
          if (i == 0) {
            errorMessage ++= "ORIGINAL TEST :\n"
            errorMessage ++= str
          }
          else if (i == tests.length - 1)
            errorMessage ++= str
        }
        prWriter.write(str.toString())
        prWriter.write("------------------------------\n")
      }
    }
  }

  private[this] def recordPassFailTests(returnValues: Array[Array[Set[Int]]], b: List[BranchOp], result: Boolean): Unit = {
    val init: Array[Set[Int]] = returnValues(0)
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val bugFreeReducedDomains: Array[Set[Int]] = returnValues(2)
    if (b != null && b.size == 1) {
      nbTestCases += 1
      testsIncPassed = testsIncPassed :+ storedResults.clone()
      storedResults = Array()
    }
    if (result) {
      if (b == null)
        testsPassed = testsPassed :+ (init, bugFreeReducedDomains, reducedDomains)
      else {
        storedResults = storedResults :+ (b.head, init, bugFreeReducedDomains)
      }
    }
    else {
      nbFailedTests += 1
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

  def updateStats(returnValues: Array[Array[Set[Int]]], b: List[BranchOp], result: Boolean): Unit = {
    nbExecutedTests += 1
    val init: Array[Set[Int]] = returnValues(0)
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val bugFreeReducedDomains: Array[Set[Int]] = returnValues(2)
    if ((bugFreeReducedDomains zip init).forall(x => x._1.equals(x._2)))
      algo1FilterNoVal += 1
    if ((bugFreeReducedDomains zip reducedDomains).forall(x => x._1.equals(x._2))) {
      algo1Equals2 += 1
    }
    else if ((bugFreeReducedDomains zip reducedDomains).forall(x => x._2.subsetOf(x._1))) {
      // algo1 filters more than algo2
      algo2FilterMoreThan1 += 1
    }
    else if ((bugFreeReducedDomains zip reducedDomains).forall(x => x._1.subsetOf(x._2))) {
      // algo2 filters more than algo1
      algo1FilterMoreThan2 += 1
    }
    else { // no relation founds. algo1 and algo2 filter differently without order relation
      algo1FilterDiffThan2 += 1
    }
    if (bugFreeReducedDomains.forall(_.size == 1)) {
      algo1FoundsInstantiation += 1
      if (reducedDomains.forall(_.size == 1))
        algo2FoundsInstantiation += 1
    }
    else if (bugFreeReducedDomains.exists(_.isEmpty)) {
      algo1FoundsNoSol += 1
      if (reducedDomains.exists(_.isEmpty))
        algo2FoundsNoSol += 1
    }
    recordPassFailTests(returnValues, b, result)
  }

}
