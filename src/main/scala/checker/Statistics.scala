package checker

import java.io._

import checker.incremental.{BranchOp, Pop, Push, RestrictDomain}

/**
  * This class is responsible of the output of CPChecker.
  * It contains various information about the execution of the tests (which algorithm
  * filters more than the other, ...)
  * It allows the creation of three output files in out/statistics :
  * 'statistics.txt' containts general stats abour the execution of the tests
  * 'passedTests.txt' contains the list of the tests successfully passed
  * 'failedTests.txt' contains the list of the failed tests, with the most reduced test as first
  *
  * @param folderName : the name of the folder in which to put the output files.
  */
class Statistics(var folderName: String) {

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

  // the error message for the assertions (FilterAssert and FilterWithState asserts need
  // a new error message when a fail occurs)
  private[this] var errorMessage = new StringBuilder

  private[this] var filenameStats: File = _

  private[this] var filenamePassed: File = _

  private[this] var filenameFailed: File = _

  // stats about the generator
  private[this] var generatorUsed: TestArgs = _

  // useful information about the incremental testing (dives)
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

  /**
    * This function creates a string with the statistics of the performed tests
    *
    * @param isInc : true if the stats concern incremental tests and false otherwise
    * @return A string with a concatenation of all information about the tests
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

  /**
    * Updates the depth and lastPop variables consequently to the branch operations
    * that have been performed.
    *
    * @param b : the list of branch operations that have been performed
    */
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

  /**
    * Creates a string to be displayed when incremental checking
    *
    * @return A string containing the incremental information : the average depth reached
    *         by the dives and its maximal depth
    */
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

  /**
    * This function is responsible of the printing of all tests information in the output files
    *
    * @param isInc : true if the tests performed are incremental tests, false otherwise
    */
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

  /**
    * The function is responsible of the printing of general information
    *
    * @param isInc    : to know if the tests are incremental or not
    * @param prWriter : the printWriter attached to the file where the results should be printed
    */
  private[this] def printStats(implicit isInc: Boolean = false, prWriter: PrintWriter): Unit = {
    prWriter.write(globalStatsToString(isInc))
    if (isInc)
      prWriter.write(branchingStatsToString())

    if (!(generatorUsed == null))
      prWriter.write(generatorUsed.toString)
  }

  /**
    * This function transforms a domain into a string representation of it
    *
    * @param dom : the domain to be display
    * @return : a string representation of dom
    */
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

  /**
    * Returns an extension of the string s to be of length l (convenient for the display)
    *
    * @param s : the string to be extended
    * @param l : the length that it has to acquire
    * @return : a string containing s followed by blank characters to form a new string
    *         of length l
    */
  private[this] def extendString(s: String, l: Int): String = {
    var result: String = s
    if (s.length != l) {
      for (_ <- s.length until l)
        result += " "
    }
    result
  }


  /**
    * This function is responsible of the display of passed/failed tests
    *
    * @param prWriter : the print writer attached to the file where the tests should be printed
    * @param tests    : the list of tests to be printed
    * @param isInc    : to know if the tests are incremental or not
    * @param passed   : to know if the test has been passed or not
    * @return : a string builder with the error message for the assertions
    */
  private[this] def printTests(prWriter: PrintWriter, tests: Array[(Array[Set[Int]], Array[Set[Int]], Array[Set[Int]])], isInc: Boolean = false, passed: Boolean = true): StringBuilder = {
    val errMsg: StringBuilder = new StringBuilder

    // tests are displayed in the reverse order in order to display first the most reduced ones
    // and then the others
    for (i <- tests.length - 1 to 0 by -1) {
      val (init, bugFreeDom, testedDom) = tests(i)

      // title for the display of non incremental tests
      var bugFreeTitle: String = "Trusted filtering "
      var testedTitle: String = "Tested filtering "

      // new titles for the display of incremental tests (no filtering in the name now
      // since it can be the domains resulting in a pop operation => no filtering)
      if (isInc) {
        bugFreeTitle = "Trusted domains "
        testedTitle = "Tested domains "
      }

      // the length for the printing in a column (others string will be extended to reach this length)
      var maxLength: Int = bugFreeTitle.length
      init.foreach(t => if (t.size > maxLength) maxLength = domainToString(t).length)
      var tested: Array[Set[Int]] = testedDom.clone()
      if (tested != null && testedDom.length < init.length)
        tested = tested ++ Array.fill(init.length - tested.length)(null)
      // string for the display
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
      if (!isInc) prWriter.write(str.toString())

      // creation of the error message for the assertions
      if (!passed) {
        if (i == tests.length - 1) errMsg ++= "TEST FAILED FOR : \n" + str
        else if (i == 0) errMsg ++= "ORIGINAL TEST FAILED : \n" + str
      }
    }
    errMsg
  }

  /**
    * Creation of a string representing all domains to be printed on a single line
    *
    * @param domains : the domains to be printed
    * @return : a string with all domains on a single line within []
    */
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

  /**
    * The printing on incremental tests over the 'passedTests.txt' file or the 'failedTests.txt' file
    *
    * @param prWriter : the print writer representing the file to be printed on
    * @param tests    : the list of tests to be printed
    * @param passed   : to know if the test have been passed successfully or not
    */
  private[this] def printIncStats(prWriter: PrintWriter, tests: Array[Array[(BranchOp, Array[Set[Int]], Array[Set[Int]])]], passed: Boolean = true): Unit = {
    if (tests.isEmpty) {
      return
    }
    var lastTest: Array[Set[Int]] = null

    // tests are displayed in the reverse order in order to display first the most reduced ones
    // and then the others
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

          // setting of the error message for the assertions
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

  /**
    *
    * @param returnValues : the tuple (init, reducedDomains, bugFreeReducedDomains) returned when making a test
    * @param b            : the list of eventual branch operations if the test is an incremental one
    * @param result       : to know if the test has been successfully passed or not
    */
  private[this] def recordPassFailTests(returnValues: Array[Array[Set[Int]]], b: List[BranchOp], result: Boolean): Unit = {
    val init: Array[Set[Int]] = returnValues(0)
    val reducedDomains: Array[Set[Int]] = returnValues(1)
    val bugFreeReducedDomains: Array[Set[Int]] = returnValues(2)
    if (b != null && b.size == 1) {
      nbTestCases += 1
      testsIncPassed = testsIncPassed :+ storedResults.clone()
      storedResults = Array()
    }

    // record the test as a test succesfully passed
    if (result) {
      if (b == null)
        testsPassed = testsPassed :+ (init, bugFreeReducedDomains, reducedDomains)
      else {
        storedResults = storedResults :+ (b.head, init, bugFreeReducedDomains)
      }
    }

    // record the test as a test that has failed.
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


  /**
    * update all variables such as 'algo1FilterMoreThan2', 'algo1FilterNoVal' based on the result of the test
    *
    * @param returnValues : the tuple (init, reducedDomains, bugFreeReducedDomains) returned when making a test
    * @param b            : the list of eventual branch operations if the test is an incremental one
    * @param result       : to know if the test has been successfully passed or not
    */
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
