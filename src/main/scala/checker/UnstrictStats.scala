package checker

class UnstrictStats extends Statistics {

  private[this] var noDomainChange: Int = 0
  private[this] var nbCorrectTestsWithSolution: Int = 0

  def incNoDomainChange(): Unit = noDomainChange += 1

  def incNbCorrectTestsWithSolution(): Unit = nbCorrectTestsWithSolution += 1

  def globStatsToString(): String = {
    ""
  }

  def globalStatsToString(): String = {
    "Depending on the constraint being tested, three kinds of tests are possible : \n Tests having no solution. \n Tests reducing domains variables. \n Tests that don't reduce any domain variable \n" +
      "Here are some stats of the tests being executed : \n\n" +
      "------------------------------------------------------------ \n" +
      "Tests                  |   Passed  |   Failed  |   Total   | \n" +
      "-----------------------|-----------|-----------|-----------| \n" +
      "without solution       |" + printNumber(getNbNoSolutionTests - getNbFailedNoSolutionTests) + "|" + printNumber(getNbFailedNoSolutionTests) + "|" + printNumber(getNbNoSolutionTests) + "| \n" +
      "with solution          |" + printNumber(nbCorrectTestsWithSolution) + "|" + printNumber(getNbExecutedTests - (getNbNoSolutionTests + nbCorrectTestsWithSolution)) + "| \n" +
      "Count                  |" + printNumber(getNbExecutedTests - nbFailedTests) + "|" + printNumber(nbFailedTests) + "|" + printNumber(getNbExecutedTests) + "| \n" +
      "------------------------------------------------------------ \n"
  }

  def nbFailedTests: Int = getNbExecutedTests - getNbNoSolutionTests - nbCorrectTestsWithSolution + getNbFailedNoSolutionTests


  def strictDomainComparison(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]], init: Array[Set[Int]], result: Boolean): Unit = {
    if ((reducedDomains zip init).forall(x => x._1.equals(x._2))) {
      incNoDomainChange()
      incNbCorrectTestsWithSolution()
    }
  }

  def incorrectDomains(ourReducedDomains: Array[Set[Int]], reducedDomains: Array[Set[Int]]): Boolean = {
    if (ourReducedDomains.exists(x => x.isEmpty))
      reducedDomains.exists(x => x.size > 1) // check that if no solution can be found, either you still have unfixed variables
      // or if all variables are instantiated, you should find there is no solution
    else{
      (ourReducedDomains zip reducedDomains).exists(x => !x._1.subsetOf(x._2))
    }
  }
}
