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
    val checkAllDiff: Array[Set[Int]] => Boolean = checkConstraint(isAC,_,constraint,allDifferent)
    forAll(Gen.containerOfN[List,Set[Int]](8,Generator)){ x =>
      x.isEmpty || checkAllDiff(x.toArray)
    }.check
    val test1 = Array(Set(0,1,2),Set(0))
    checkAllDiff(test1)
    val test2 = Array(Set(0,1),Set(0,1),Set(0,1,2))
    checkAllDiff(test2)
    val test3 = Array(Set(0),Set(1),Set(2))
    checkAllDiff(test3)
    val test4 = Array(Set(0,1),Set(1,2),Set(2,3),Set(3,4),Set(4,5),Set(2,4))
    checkAllDiff(test4)
    val test5 = Array(Set(0,1,2))
    checkAllDiff(test5)
    val test6 = Array(Set(0,1,2), Set(1))
    checkAllDiff(test6)
    val test7 = Array(Set(0,1,2))
    checkAllDiff(test7)
    val test8 = Array(Set(0,1,2), Set(0), Set(1), Set(2))
    checkAllDiff(test8)
    val test9 = Array(Set(0,1,2,3,4), Set(1), Set(4), Set(3))
    checkAllDiff(test9)
    val test10 = Array(Set(0,1,2), Set(0), Set(2))
    checkAllDiff(test10)
    val test11 = Array(Set(1), Set(1), Set(5,6))
    checkAllDiff(test11)
    val test12 = Array(Set(8,16), Set(16,11), Set(7,8), Set(16), Set(15,8), Set(11,7))
    checkAllDiff(test12)
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
          println("but you had: " + reducedDomains.toList)
          return false
        }
      }
    }
    else{
      var empty = false
      reducedDomains.foreach(x => if(!empty) empty=x.isEmpty)
      if(!empty) {
        println("failed for: " + variables.toList)
        println("you should not have any solutions")
        println("but you had: " + reducedDomains.toList)
        return false //empty domains accepted as having no solutions
      }
    }
    true
  }

  def main(args: Array[String]): Unit ={
    checkAllDifferentBC(allDifferent)
    //checkAllDifferentAC(allDifferent)
    //?? not ready yet: checkSum(sum,5,Op.equal)
    //checkConstraint(false,
      //Array(Set(18, 14), Set(12, 13), Set(12), Set(12, 19)),
      //allDifferent, sum(52,Op.greaterThanOrEqual,4))
  }

}