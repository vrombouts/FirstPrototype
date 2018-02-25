package checker

import checker.Constraints._
import org.scalacheck.Prop.forAll


import scala.language.implicitConversions


trait Checker {
  def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach{x => if(x.isEmpty) return true}
    false
  }

  def printer(initial:Array[Set[Int]],trueReduced:Array[Set[Int]],reduced:Array[Set[Int]],error:Boolean,ourError:Boolean):Unit = {
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

  def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]]

  def checkConstraint(variables:Array[Set[Int]],
                      constraintTested:Array[Set[Int]]=>Array[Set[Int]])
                      : Boolean ={
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
      trueReducedDomains = applyConstraint(variables.clone())
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

}