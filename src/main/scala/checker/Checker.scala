package checker

import checker.Constraints._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll


import scala.language.implicitConversions


object Checker {
  var j = 0
  val Generator: Gen[Set[Int]] =  Gen.containerOfN[Set,Int](2,Gen.choose(0,20))
  def GeneratorVariable(): Gen[Variable] ={
    for{
      set <- Generator
    }yield new Variable(set)
  }
  def GeneratorListOfVariables(n:Int): Gen[List[Variable]]={
    Gen.containerOfN[List,Variable](21,GeneratorVariable())
  }

  def checkAllDifferentAC(constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    checkAllDifferent(true,constraint)
  }
  def checkAllDifferentBC(constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    checkAllDifferent(false,constraint)
  }
  /*
   * This function checks if the constraint passed in argument apply correctly an
   * allDifferent constraint with arc consistency.
   */
  private def checkAllDifferent(isAC: Boolean, constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {

    forAll(Gen.containerOfN[List,Set[Int]](8,Generator)){ x =>
      x.isEmpty || checkAllDiff(isAC,x.toArray,constraint)
    }.check

    val test1 = Array(Set(0,1,2),Set(0))
    checkAllDiff(isAC,test1,constraint)
    val test2 = Array(Set(0,1),Set(0,1),Set(0,1,2))
    checkAllDiff(isAC,test2,constraint)
    val test3 = Array(Set(0),Set(1),Set(2))
    checkAllDiff(isAC,test3,constraint)
    val test4 = Array(Set(0,1),Set(1,2),Set(2,3),Set(3,4),Set(4,5),Set(2,4))
    checkAllDiff(isAC,test4,constraint)
    val test5 = Array(Set(0,1,2))
    checkAllDiff(isAC,test5,constraint)
    val test6 = Array(Set(0,1,2), Set(1))
    checkAllDiff(isAC,test6,constraint)
    val test7 = Array(Set(0,1,2))
    checkAllDiff(isAC,test7,constraint)
    val test8 = Array(Set(0,1,2), Set(0), Set(1), Set(2))
    checkAllDiff(isAC,test8,constraint)
    val test9 = Array(Set(0,1,2,3,4), Set(1), Set(4), Set(3))
    checkAllDiff(isAC,test9,constraint)
    val test10 = Array(Set(0,1,2), Set(0), Set(2))
    checkAllDiff(isAC,test10,constraint)
    val test11 = Array(Set(1), Set(1), Set(5,6))
    checkAllDiff(isAC,test11,constraint)
    val test12 = Array(Set(8,16), Set(16,11), Set(7,8), Set(16), Set(15,8), Set(11,7))
    checkAllDiff(isAC,test12,constraint)
    println("All tests executed.")
  }


  private def checkAllDiff(isAC: Boolean, variables:Array[Set[Int]],constraintTested:Array[Set[Int]]=>Array[Set[Int]]): Boolean ={
    //We first compute the domains generated after the application of the constraint.
    var reducedDomains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var ourError:Boolean = false
    try {
      reducedDomains = constraintTested(variables)
    }
    catch{
      case e: Exception => error = true
    }
    // Then we generate the domains that reducedDomains should have
    var trueReducedDomains: Array[Set[Int]] = Array()
    try {
      if(isAC)
        trueReducedDomains = applyAC(variables, allDifferent1)
      else
        trueReducedDomains = applyBC(variables, allDifferent1)
    }
    catch {
      case e: Exception => ourError = true
    }
      //Finally, we compare the two. If they are not equals, the constraint is not correct.
    if(error && ourError) return true

      if(error && !ourError){
        for(i<- reducedDomains.indices){
          if(!reducedDomains(i).isEmpty){
            println("failed for: "+ variables.toList)
            println("you should have: "+ trueReducedDomains.toList)
            println("but you returned an exception")
            return false
          }
        }
        return true
      }
      else if(!ourError) {
        for (i <- trueReducedDomains.indices) {
          if (!trueReducedDomains(i).equals(reducedDomains(i))) {
            println("failed for: " + variables.toList)
            println("you should have: " + trueReducedDomains.toList)
            println("but you had " + reducedDomains.toList)
            return false
          }
        }
      }
    else{
        return false
    }
      true
    }


  def main(args: Array[String]): Unit ={
    checkAllDifferentBC(allDifferent)

    checkAllDiff(false,Array(Set(18, 14), Set(12, 13), Set(12), Set(12, 19)),allDifferent)
  }

}