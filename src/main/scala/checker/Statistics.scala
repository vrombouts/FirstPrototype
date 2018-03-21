package checker
import java.io._

object Statistics {

  // stats about the number of executed tests
  private var nbExecutedTests:Int=0
  private var nbNoSolutionTests:Int=0
  private var nbRemoveNoValueTests:Int=0
  private var nbRemovingValueTests:Int=0
  private var nbFailedTests:Int=0
  private var nbFailedNoSolutionTests:Int=0
  private var nbFailedRemoveNoValueTests:Int=0
  private var nbFailedRemovingValueTests:Int=0

  private var nbBacktracks:Int=0
  private var nbNodes:Int=0
  private var nbLeaves:Int=0

  // stats about the generator
  private var nbVarsConsidered:Int=0
  private var range:(Int,Int)= (0,0)
  private var density:Double = 0.0

  private var lastTestFail:(Array[Set[Int]],Array[Set[Int]],Array[Set[Int]])=(null,null,null)

  def incNbExecutedTests() : Unit = nbExecutedTests += 1
  def incNbNoSolutionTests() : Unit = nbNoSolutionTests += 1
  def incNbRemoveNoValueTests() : Unit = nbRemoveNoValueTests += 1
  def incNbRemovingValueTests() : Unit = nbRemovingValueTests += 1
  def incNbFailedNoSolutionTests() : Unit = nbFailedNoSolutionTests += 1
  def incNbFailedRemoveNoValueTests() : Unit = nbFailedRemoveNoValueTests += 1
  def incNbFailedRemovingValueTests() : Unit = nbFailedRemovingValueTests += 1
  def incNbFailedTests() : Unit = nbFailedTests += 1
  def incNbBacktracks() : Unit = nbBacktracks += 1
  def incNbNodes() : Unit = nbNodes += 1
  def incNbLeaves() : Unit = nbLeaves += 1

  def globStatsToString():String={
    "The total number of tests that have been executed is "+nbExecutedTests+"\n"+
    "The number of tests that had no solution is "+nbNoSolutionTests + "/"+nbExecutedTests+" ("+nbFailedNoSolutionTests+" failed)"+"\n"+
    "The number of tests that didn't change anything in the domains compared to the initial is "+nbRemoveNoValueTests+"/"+nbExecutedTests+" (" + nbFailedRemoveNoValueTests+" failed) "+"\n"+
    "The number of tests that reduced the domains but still have a solution is "+nbRemovingValueTests+"/"+nbExecutedTests+" ("+ nbFailedRemovingValueTests+ " failed)"+"\n"+
    "The number of tests that you pass successfully is "+(nbExecutedTests-nbFailedTests)+"/"+nbExecutedTests+"\n"+
    "The number of tests that you fail "+ nbFailedTests +"/"+nbExecutedTests+"\n"
  }

  def globalStatsToString():String={
    "Depending on the constraint being tested, three kinds of tests are possible : \n Tests having no solution. \n Tests reducing domains variables. \n Tests that don't reduce any domain variable \n"+
    "Here are some stats of the tests being executed : \n\n"+
    "------------------------------------------------------------ \n"+
    "Tests                  |   Passed  |   Failed  |   Total   | \n"+
    "-----------------------|-----------|-----------|-----------| \n"+
    "without solution       |"+printNumber(nbNoSolutionTests-nbFailedNoSolutionTests)+"|"+printNumber(nbFailedNoSolutionTests)+"|"+printNumber(nbNoSolutionTests)+"| \n"+
    "with domain reduction  |"+printNumber(nbRemovingValueTests-nbFailedRemovingValueTests)+"|"+printNumber(nbFailedRemovingValueTests)+"|"+printNumber(nbRemovingValueTests)+"| \n"+
    "with no reduction      |"+printNumber(nbRemoveNoValueTests-nbFailedRemoveNoValueTests)+"|"+printNumber(nbFailedRemoveNoValueTests)+"|"+printNumber(nbRemoveNoValueTests)+"| \n"
  }

  def printNumber(nb:Int):String={
    if(nb==0) return " "+0+"         "
    if(nb==1) return " "+1+"         "
    var nbOfChars:Int=nb.toString.size
    var s:String=" "+nb
    for(i <- 1 to 10-nbOfChars){
      s=s+" "
    }
    s
  }


  def branchingStatsToString():String={
    "The average number of backtracks per test is "+nbBacktracks/nbExecutedTests +"\n"+
    "The average number of nodes per test is "+nbNodes/nbExecutedTests +"\n"+
    "The average number of leaves per test is "+nbLeaves/nbExecutedTests +"\n"
  }

  def printStats(implicit isInc:Boolean=false):Unit={
    val prWriter = new PrintWriter(new File("out/statistics.txt"))
    prWriter.write(globalStatsToString())
    if(isInc) {
      prWriter.write(branchingStatsToString())
    }
    prWriter.write("------------------------------------------------------------\n")
    prWriter.close()
  }
}
