package checker.statistics

class CheckStatistics(filename: String) extends Statistics("check/" + filename) {

  def this() = this("")

  private[this] var nbRemoveNoValueTests: Int = 0
  private[this] var nbRemovingValueTests: Int = 0
  private[this] var nbFailedRemoveNoValueTests: Int = 0
  private[this] var nbFailedRemovingValueTests: Int = 0

  override def setFileName(filename: String): Unit = super.setFileName("check/" + filename)

  def incNbRemoveNoValueTests(): Unit = nbRemoveNoValueTests += 1

  def incNbRemovingValueTests(): Unit = nbRemovingValueTests += 1

  def incNbFailedRemoveNoValueTests(): Unit = nbFailedRemoveNoValueTests += 1

  def incNbFailedRemovingValueTests(): Unit = nbFailedRemovingValueTests += 1

  def getNbRemoveNoValueTests: Int = nbRemoveNoValueTests

  def getNbRemovingValueTests: Int = nbRemovingValueTests

  def getNbFailedRemoveNoValueTests: Int = nbFailedRemoveNoValueTests

  def getNbFailedRemovingValueTests: Int = nbFailedRemovingValueTests


  def nbFailedTests: Int = nbFailedRemoveNoValueTests + nbFailedRemovingValueTests + getNbFailedNoSolutionTests

  /*
    def globStatsToString(): String = {
      "The total number of tests that have been executed is " + getNbExecutedTests + "\n" +
        "The number of tests that had no solution is " + getNbNoSolutionTests + "/" + getNbExecutedTests + " (" + getNbFailedNoSolutionTests + " failed)" + "\n" +
        "The number of tests that didn't change anything in the domains compared to the initial is " + nbRemoveNoValueTests + "/" + getNbExecutedTests + " (" + nbFailedRemoveNoValueTests + " failed) " + "\n" +
        "The number of tests that reduced the domains but still have a solution is " + nbRemovingValueTests + "/" + getNbExecutedTests + " (" + nbFailedRemovingValueTests + " failed)" + "\n" +
        "The number of tests that you pass successfully is " + (getNbExecutedTests - nbFailedTests) + "/" + getNbExecutedTests + "\n" +
        "The number of tests that you fail " + nbFailedTests + "/" + getNbExecutedTests + "\n"
    }
  */

  def globalStatsToString(isInc: Boolean): String = {
    var str: String = "Depending on the constraint being tested, three kinds of tests are possible : \n Tests having no solution. \n Tests reducing domains variables. \n Tests that don't reduce any domain variable \n"
    if (isInc) str += "Note that since we make " + " branchings per test, the total number of tests will be >= " + generatorUsed.getNbTests + "\n"
    str + "Here are some stats of the tests being executed : \n\n" +
      "------------------------------------------------------------ \n" +
      "Comparisons            |   Passed  |   Failed  |   Total   | \n" +
      "-----------------------|-----------|-----------|-----------| \n" +
      "without solution       |" + printNumber(getNbNoSolutionTests - getNbFailedNoSolutionTests) + "|" + printNumber(getNbFailedNoSolutionTests) + "|" + printNumber(getNbNoSolutionTests) + "| \n" +
      "with domain reduction  |" + printNumber(nbRemovingValueTests - nbFailedRemovingValueTests) + "|" + printNumber(nbFailedRemovingValueTests) + "|" + printNumber(nbRemovingValueTests) + "| \n" +
      "with no reduction      |" + printNumber(nbRemoveNoValueTests - nbFailedRemoveNoValueTests) + "|" + printNumber(nbFailedRemoveNoValueTests) + "|" + printNumber(nbRemoveNoValueTests) + "| \n" +
      "Count                  |" + printNumber(getNbExecutedTests - nbFailedTests) + "|" + printNumber(nbFailedTests) + "|" + printNumber(getNbExecutedTests) + "| \n" +
      "------------------------------------------------------------ \n"
  }

  def strictDomainComparison(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]], init: Array[Set[Int]], result: Boolean): Unit = {

    if ((ourReducedDomains zip init).forall(x => x._1.equals(x._2))) {
      incNbRemoveNoValueTests()
      if (!result) incNbFailedRemoveNoValueTests()
    }
    else {
      incNbRemovingValueTests()
      if (!result) incNbFailedRemovingValueTests()
    }
  }

  def correctDomains(solutionDoms: Array[Set[Int]], userReducedDomains: Array[Set[Int]]): Boolean = {
    if (solutionDoms.length != userReducedDomains.length) return false
    if (solutionDoms.exists(_.isEmpty) && userReducedDomains.forall(_.nonEmpty)) return false
    !(solutionDoms zip userReducedDomains).exists(x => !x._1.equals(x._2))
  }
}
