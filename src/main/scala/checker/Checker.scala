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
  def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach{x => if(x.isEmpty) return true}
    false
  }

  def checkAC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    val filteringAC: Array[Set[Int]] => Array[Set[Int]] = applyAC(_,checker)
    forAll(Gen.containerOfN[List,Set[Int]](8,Generator)){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringAC, filteringTested)
    }.check
    //TODO: add simple case limit possible for all constraints
  }

  def checkBC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    val filteringBC: Array[Set[Int]] => Array[Set[Int]] = applyBC(_,checker)
    forAll(Gen.containerOfN[List,Set[Int]](8,Generator)){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringBC, filteringTested)
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
    val checkAllDiff: Array[Set[Int]] => Boolean = checkConstraint(_,propagation,constraint)
    LimitCases.allDifferentLimitCases().foreach(x => checkAllDiff(x))
    println("All tests executed.")
  }

  def checkSum(constraint:Array[Set[Int]]=>Array[Set[Int]],constant:Int, operation:Int):Unit = {

    forAll(Gen.containerOfN[List,Set[Int]](8,Generator)){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray,applyBC(_,sum(constant,Op.equal,x.size)),constraint)
    }.check
  }

  def checkSummation(constraint:Array[Set[Int]] => Array[Set[Int]],constant:Int,operation:Int):Unit={
    val sum: (Array[Set[Int]]) => Array[Set[Int]] = sumBC(_,constant,operation)
    val check: (Array[Set[Int]]) => Boolean = checkConstraint(_,sum,constraint)
    forAll(Gen.containerOfN[List,Set[Int]](20,Generator)){ x =>
      x.isEmpty || checkEmpty(x) || check(x.toArray)
    }.check
    val limitCase1 = Array(Set(Integer.MAX_VALUE), Set(2))
    check(limitCase1)
    val limitCase2 = Array(Set(Integer.MIN_VALUE), Set(-2))
    check(limitCase2)
    val limitCase3 = Array(Set(2), Set(constant-2))
    check(limitCase3)
    val limitCase4 = Array(Set(2,constant-2),Set(2,constant-2))
    check(limitCase4)
    val limitCase5 = Array(Set(1,2), Set(2,4),Set(constant-6)) // constant == sMax
    check(limitCase5)
    val limitCase6 = Array(Set(1,2), Set(2,4),Set(constant-5)) //constant > sMax
    check(limitCase6)
    val limitCase7 = Array(Set(1,2), Set(2,4),Set(constant-7)) // constant < sMax
    check(limitCase7)
    val limitCase8 = Array(Set(1,2), Set(2,4),Set(constant-3)) // constant == sMin
    check(limitCase8)
    val limitCase9 = Array(Set(1,2), Set(2,4),Set(constant-2))   //constant > sMin
    check(limitCase9)
    val limitCase10 = Array(Set(1,2), Set(2,4),Set(constant-4)) // constant < sMin
    check(limitCase10)
    val limitCase11 = Array(Set(2), Set(2),Set(constant-4)) // constant == sMin == sMax
    check(limitCase11)
  }
  private def checkConstraint(variables:Array[Set[Int]],
                              constraint : (Array[Set[Int]])=> Array[Set[Int]],
                              constraintTested:Array[Set[Int]]=>Array[Set[Int]]): Boolean ={
    //We first compute the domains generated after the application of the constraint.
    var reducedDomains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var ourError:Boolean = false
    try {
      reducedDomains = constraintTested(variables.clone())
    }
    catch{
      case e: Exception => error = true
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
      for(i<- reducedDomains.indices){
        if(reducedDomains(i).nonEmpty){
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
      //empty domains accepted as having no solutions
      if(reducedDomains.nonEmpty && reducedDomains.forall(x=> x.nonEmpty)){
        println("failed for: " + variables.toList)
        println("you should not have any solutions")
        println("but you had: " + reducedDomains.toList)
        return false
      }
    }
    true
  }

  def main(args: Array[String]): Unit ={
    //checkAllDifferentBC(allDifferent)
    //checkAllDifferentAC(allDifferent)
    //?? not ready yet: checkSum(sum,5,Op.equal)
    //checkConstraint(false,
      //Array(Set(18, 14), Set(12, 13), Set(12), Set(12, 19)),
      //allDifferent, sum(52,Op.greaterThanOrEqual,4))
    //var res=sumBC(Array(Set(1,5), Set(12), Set(1,2)), 25, Op.greaterThan)
    //println(res.toList)
    //checkSummation(dummyConstraint,50, Op.equal)

    var acts:Array[Activity] = Array(new Activity(Set(0), Set(2), Set(2)), new Activity(Set(0),Set(2), Set(2)))
    val un = new UnaryResource
    println(un.overloadChecking(acts))
    //println(sumBC(Array(Set(2), Set(2),Set(46)), 50,Op.equal).toList)
  }

}