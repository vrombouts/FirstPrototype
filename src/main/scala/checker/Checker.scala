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
      x.isEmpty || checkConstraint(isAC,x.toArray,constraint, allDifferent)
    }.check

    val test1 = Array(Set(0,1,2),Set(0))
    checkConstraint(isAC,test1,constraint,allDifferent)
    val test2 = Array(Set(0,1),Set(0,1),Set(0,1,2))
    checkConstraint(isAC,test2,constraint, allDifferent)
    val test3 = Array(Set(0),Set(1),Set(2))
    checkConstraint(isAC,test3,constraint, allDifferent)
    val test4 = Array(Set(0,1),Set(1,2),Set(2,3),Set(3,4),Set(4,5),Set(2,4))
    checkConstraint(isAC,test4,constraint, allDifferent)
    val test5 = Array(Set(0,1,2))
    checkConstraint(isAC,test5,constraint, allDifferent)
    val test6 = Array(Set(0,1,2), Set(1))
    checkConstraint(isAC,test6,constraint, allDifferent)
    val test7 = Array(Set(0,1,2))
    checkConstraint(isAC,test7,constraint, allDifferent)
    val test8 = Array(Set(0,1,2), Set(0), Set(1), Set(2))
    checkConstraint(isAC,test8,constraint, allDifferent)
    val test9 = Array(Set(0,1,2,3,4), Set(1), Set(4), Set(3))
    checkConstraint(isAC,test9,constraint, allDifferent)
    val test10 = Array(Set(0,1,2), Set(0), Set(2))
    checkConstraint(isAC,test10,constraint, allDifferent)
    val test11 = Array(Set(1), Set(1), Set(5,6))
    checkConstraint(isAC,test11,constraint, allDifferent)
    val test12 = Array(Set(8,16), Set(16,11), Set(7,8), Set(16), Set(15,8), Set(11,7))
    checkConstraint(isAC,test12,constraint, allDifferent)
    println("All tests executed.")
  }

  def checkSum(constraint:Array[Set[Int]]=>Array[Set[Int]],constant:Int, operation:Int):Unit = {
    forAll(Gen.containerOfN[List,Set[Int]](8,Generator)){ x =>
      x.isEmpty || checkConstraint(false,x.toArray,constraint,sum(constant,Op.equal,x.size))
    }.check
  }

  private def checkConstraint(isAC: Boolean,
                      variables:Array[Set[Int]],
                      constraintTested:Array[Set[Int]]=>Array[Set[Int]],
                      checkerOfConstraint:Array[Int]=>Boolean): Boolean ={
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
        trueReducedDomains = applyAC(variables, checkerOfConstraint)
      else
        trueReducedDomains = applyBC(variables, checkerOfConstraint)
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
    //checkAllDifferentBC(allDifferent)

    checkConstraint(false,
      Array(Set(18, 14), Set(12, 13), Set(12), Set(12, 19)),
      allDifferent, sum(52,Op.greaterThanOrEqual,4))
  }

}