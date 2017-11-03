package checker

import checker.Constraints._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll


import scala.language.implicitConversions


object Checker {
  var j = 0
  val Generator: Gen[Set[Int]] =  Gen.containerOfN[Set,Int](2,Gen.choose(0,20))
  def Generator_variable(): Gen[Variable] ={
    for{
      set <- Generator
    }yield new Variable(set)
  }
  def Generator_List_Of_Variables(n:Int): Gen[List[Variable]]={
    Gen.containerOfN[List,Variable](21,Generator_variable())
  }
  /*
   * This function check if the constraint passed in argument apply correctly an
   * allDifferent constraint with arc consistency.
   */
  def check_AllDifferent(constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    forAll(Gen.containerOfN[List,Set[Int]](8,Generator)){ x =>
      x.isEmpty || check_AllDiff(x.toArray,constraint)
    }.check

    val test1 = Array(Set(0,1,2),Set(0))
    check_AllDiff(test1,constraint)
    val test2 = Array(Set(0,1),Set(0,1),Set(0,1,2))
    check_AllDiff(test2,constraint)
    val test3 = Array(Set(0),Set(1),Set(2))
    check_AllDiff(test3,constraint)
    val test4 = Array(Set(0,1),Set(1,2),Set(2,3),Set(3,4),Set(4,5),Set(2,4))
    check_AllDiff(test4,constraint)
    val test5 = Array(Set(0,1,2))
    check_AllDiff(test5,constraint)
    println("finish")
  }

  def check_AllDiff(variables:Array[Set[Int]],constraint_tested:Array[Set[Int]]=>Array[Set[Int]]): Boolean ={
    //We first compute the domains generated after the application of the constraint.
    var reduced_domains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var our_error:Boolean = false
    try {
      reduced_domains = constraint_tested(variables)
    }
    catch{
      case e: Exception => error = true
    }
    // Then we generate the domains that reduced_domains should have
    var true_reduced_domains: Array[Set[Int]] = Array()
    try {
      true_reduced_domains = cartesian_product(variables, AllDifferent1)
    }
    catch {
      case e: Exception => our_error = true
    }

      //Finally, we compare the two. If they are not equals, the constraint is not correct.
    if(error && our_error) return true

      if(error && !our_error){
        for(i<- reduced_domains.indices){
          if(!reduced_domains(i).isEmpty){
            println("failed for: "+ variables.toList)
            println("you should have: "+ true_reduced_domains.toList)
            println("but you returned an exception")
            return false
          }
        }
        return true
      }
      else if(!our_error) {
        for (i <- true_reduced_domains.indices) {
          if (!true_reduced_domains(i).equals(reduced_domains(i))) {
            println("failed for: " + variables.toList)
            println("you should have: " + true_reduced_domains.toList)
            println("but you had " + reduced_domains.toList)
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
    check_AllDifferent(AllDifferent)
  }

}