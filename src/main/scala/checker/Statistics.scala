package checker
import java.io._

object Statistics {

  // stats about the number of executed tests
  private var nbExecutedTests:Int=0
  private var nbNoSolutionTests:Int=0
  private var nbRemoveNoValueTests:Int=0
  private var nbRemovingValueTests:Int=0

  // stats about the generator
  private var nbVarsConsidered:Int=0
  private var range:(Int,Int)= (0,0)
  private var density:Double = 0.0

  def incNbExecutedTests() : Unit = nbExecutedTests += 1
  def incNbNoSolutionTests() : Unit = nbNoSolutionTests += 1
  def incNbRemoveNoValueTests() : Unit = nbRemoveNoValueTests += 1
  def incNbRemovingValueTests() : Unit = nbRemovingValueTests += 1

  def statisticsToString():String={
    "The total number of tests that have been executed is "+nbExecutedTests+"\n"+
    "The number of tests that had no solution is "+nbNoSolutionTests + "/"+nbExecutedTests+"\n"+
    "The number of tests that didn't change anything in the domains compared to the initial is "+nbRemoveNoValueTests+"/"+nbExecutedTests+"\n"+
    "The number of tests that reduced the domains but still have a solution is "+nbRemovingValueTests+"/"+nbExecutedTests
  }

  def printStats():Unit={
    val prWriter = new PrintWriter(new File("statistics.txt"))
    prWriter.write(statisticsToString())
    prWriter.close
  }
}
