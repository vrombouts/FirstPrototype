package checker

import checker.Constraints._
import org.scalacheck.Prop.forAll


import scala.language.implicitConversions


object Checker {
  def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach{x => if(x.isEmpty) return true}
    false
  }

  def checkAC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    val filteringAC: Array[Set[Int]] => Array[Set[Int]] = applyAC(_,checker)
    forAll(Generators.basic){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringAC, filteringTested,basicPrinter)
    }.check
    //TODO: add simple case limit possible for all constraints
  }

  def checkBC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    val filteringBC: Array[Set[Int]] => Array[Set[Int]] = applyBC(_,checker)
    forAll(Generators.basic){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringBC, filteringTested,basicPrinter)
    }.check
    //TODO: add simple case limit possible for all constraints
  }

  /*
   * This function checks if the constraint passed in argument apply correctly an
   * allDifferent constraint with arc consistency.
   */
  def checkAllDifferent(isAC: Boolean, constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    var propagation: (Array[Set[Int]]) => Array[Set[Int]] = applyBC(_,allDifferent)
    if(isAC) {
      checkAC(constraint,allDifferent)
      propagation = applyAC(_,allDifferent)
    }
    else checkBC(constraint,allDifferent)
    val checkAllDiff: Array[Set[Int]] => Boolean = checkConstraint(_,propagation,constraint,basicPrinter)
    LimitCases.allDifferentLimitCases.foreach(x => checkAllDiff(x))
    println("All tests executed.")
  }


  def checkGcc(constraint: (Array[Set[Int]],Array[Int])=>Array[Set[Int]]): Unit = {
    val check: Array[Set[Int]] => Boolean = checkConstraint(_,gcc(_,Array(0,1,2)),constraint(_, Array(0,1,2)),basicPrinter)
    forAll(Generators.gcc) { variables =>
      variables.isEmpty || checkEmpty(variables) || variables.length<4 || check(variables.toArray)
    }.check
    LimitCases.gccLimitCases.foreach{limit =>
      checkConstraint(limit._1,gcc(_,limit._2), constraint(_,limit._2),basicPrinter)
    }
  }

  def checkSum(constraint:Array[Set[Int]]=>Array[Set[Int]],constant:Int, operation:Int):Unit = {

    forAll(Generators.basic){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray,applyBC(_,sum(constant,Op.equal,x.size)),constraint,basicPrinter)
    }.check
  }

  def checkSummation(constraint:Array[Set[Int]] => Array[Set[Int]],operation:Int):Unit={
    val sum: (Array[Set[Int]]) => Array[Set[Int]] = sumBC(_,operation)
    val check: (Array[Set[Int]]) => Boolean = checkConstraint(_,sum,constraint,basicPrinter)
    forAll(Generators.sum){ x =>
      if(x._1.isEmpty || checkEmpty(x._1)) true
      else{
        val array = x._1.toArray
        check(array++Array(Set(x._2)))
      }
    }.check
    LimitCases.sumLimitCases.foreach{limitCase => check(limitCase)}
  }

  def printTable(table: Set[Array[Int]]):Unit = {
    println("With Table : ")
    for(x <- table){
      print("[ ")
      x.foreach(y=>print(" "+y))
      println("]")
    }
    println()
  }

  def checkTable(constraint: (Array[Set[Int]],Set[Array[Int]])=>Array[Set[Int]]): Unit= {
    forAll(Generators.table) { list =>
      val variables = list._1
      val table = list._2
      if(variables.isEmpty || checkEmpty(variables) || table.isEmpty) true
      else if(!checkConstraint(variables.toArray, tableAC(_, table), constraint(_, table),basicPrinter)){
        printTable(table)
        false
      }else true
    }.check
    LimitCases.tableLimitCases.foreach(x =>
      if(!checkConstraint(x._1, tableAC(_,x._2), constraint(_,x._2),basicPrinter))
        printTable(x._2))
  }

  def checkElementAC(constraint: Array[Set[Int]] => Array[Set[Int]]) : Unit={
    val check: (Array[Set[Int]]) => Boolean = checkConstraint(_,elementAC,constraint,elementPrinter)
    forAll(Generators.element){ x =>
      val X :Array[Set[Int]] = x._1.toArray
      val i:Set[Int] = x._2
      val v:Set[Int] = x._3
      val variables:Array[Set[Int]] = X ++ Array(i,v)
       checkEmpty(variables.toList) || X.length <= 0 || i.size <= 0 || v.size <=0 || check(variables) //x.length<2
    }.check
    LimitCases.elementLimitCases.foreach(limitCase => {check(limitCase)})
  }

  def elementPrinter(initial:Array[Set[Int]],trueReduced:Array[Set[Int]],reduced:Array[Set[Int]],error:Boolean,ourError:Boolean):Unit = {
    val init = initial.dropRight(2).toList
    val initI = initial(initial.length-2)
    val initV = initial(initial.length-1)
    val tr = trueReduced.dropRight(2)
    val trI = trueReduced(trueReduced.length-2)
    val trV = trueReduced(trueReduced.length-1)
    val r = reduced.dropRight(2)
    val rI = reduced(reduced.length-2)
    val rV = reduced(reduced.length-1)
    if(error && !ourError) {
      println("failed with X domains: " + initial.toList + "\n i domain: " + initI + "\n v domain: " +initV)
      println("you should have: " + trueReduced.toList+ "\n i domain: " + trI + "\n v domain: " +trV)
      println("but you returned an exception")
    }else if(!error && ourError){
      println("failed with X domains: " + initial.toList+ "\n i domain: " + initI + "\n v domain: " +initV)
      println("you should not have any solutions")
      println("but you had: " + reduced.toList+ "\n i domain: " + rI + "\n v domain: " +rV)
    }else if(!error && !ourError){
      println("failed for: " + initial.toList+ "\n i domain: " + initI + "\n v domain: " +initV)
      println("you should have: " + trueReduced.toList+ "\n i domain: " + trI + "\n v domain: " +trV)
      println("but you had: " + reduced.toList+ "\n i domain: " + rI + "\n v domain: " +rV)
    }
  }

  def basicPrinter(initial:Array[Set[Int]],trueReduced:Array[Set[Int]],reduced:Array[Set[Int]],error:Boolean,ourError:Boolean):Unit = {
    if(error && !ourError) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you returned an exception")
    }else if(!error && ourError){
      println("failed for: " + initial.toList)
      println("you should not have any solutions")
      println("but you had: " + reduced.toList)
    }else if(!error && !ourError){
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you had: " + reduced.toList)
    }
  }

  private def checkConstraint(variables:Array[Set[Int]],
                              constraint : (Array[Set[Int]])=> Array[Set[Int]],
                              constraintTested:Array[Set[Int]]=>Array[Set[Int]],
                              printer: (Array[Set[Int]],Array[Set[Int]],Array[Set[Int]],
                                Boolean,Boolean)=>Unit): Boolean ={
    //We first compute the domains generated after the application of the constraint.
    var reducedDomains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var ourError:Boolean = false
    try {
      reducedDomains = constraintTested(variables.clone())
    }
    catch{
      //TODO check if it is not better to have a case of NoSolutionException instead
      case e: NoSolutionException => error = true
    }
    // Then we generate the domains that reducedDomains should have
    var trueReducedDomains: Array[Set[Int]] = Array()
    try {
      trueReducedDomains = constraint(variables.clone())
    }
    catch {
      case e: NoSolutionException => ourError = true
    }
    //Finally, we compare the two. If they are not equals, the constraint is not correct.
    if(error && ourError) return true

    if(error && !ourError){
      for(i<- trueReducedDomains.indices){
        if(trueReducedDomains(i).nonEmpty){
          printer(variables,trueReducedDomains,reducedDomains,error,ourError)
          return false
        }
      }
    }
    else if(!ourError) {
      for (i <- trueReducedDomains.indices) {
        if (!trueReducedDomains(i).equals(reducedDomains(i))) {
          printer(variables,trueReducedDomains,reducedDomains,error,ourError)
          return false
        }
      }
    }
    else{
      //empty domains accepted as having no solutions
      if(reducedDomains.nonEmpty && reducedDomains.forall(x=> x.nonEmpty)){
        printer(variables,trueReducedDomains,reducedDomains,error,ourError)
        return false
      }
    }
    true
  }

  def main(args: Array[String]): Unit ={
    //checkAllDifferentBC(allDifferent)
    //checkAllDifferent(isAC = true,allDifferent)
    //?? not ready yet: checkSum(sum,5,Op.equal)
    //checkConstraint(false,
      //Array(Set(18, 14), Set(12, 13), Set(12), Set(12, 19)),
      //allDifferent, sum(52,Op.greaterThanOrEqual,4))
    //var res=sumBC(Array(Set(1,5), Set(12), Set(1,2)), 25, Op.greaterThan)
    //println(res.toList)
    //checkTable({(vari,seti) => vari})
    //checkGcc({(v,s)=>v})
    //var acts:Array[Activity] = Array(new Activity(Set(0), Set(2), Set(2)), new Activity(Set(0),Set(2), Set(2)))
    //val un = new UnaryResource
    //println(un.overloadChecking(acts))
    //println(sumBC(Array(Set(2), Set(2),Set(46)), 50,Op.equal).toList)
    checkElementAC(dummyConstraint)
  }

}