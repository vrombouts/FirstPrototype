package checker
import java.util

import checker.Constraints._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.collection.JavaConverters._
import scala.collection.mutable;


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

  /*def check_AllDifferent(Body : List[Variable] => Unit): Unit = {
    val n = 10
    forAll(Generator_List_Of_Variables(n)){x =>
      check_AllDifferent(x,Body)
    }.check
    //add limit cases
  }*/

  def check_AllDifferent(variables:List[Variable],body:List[Variable] => Unit): Boolean ={
    val result = generate_solutions(variables, AllDifferent)
    println(result)
    println("#################################")
    body(variables)
    println(variables)
    for (i <- result.indices) {
      if (!result(i).same_domain(variables(i))) {
        return false
      }
    }
    true
  }
//////////////////////////////////////////////////////////////
  def scToJ_Set(set:Set[Int]):util.Set[Integer] = {
    val a: util.Set[Integer] = new util.HashSet[Integer]()
    for(i<- set){
      a.add(i)
    }
    a
  }
  def jToSc_Set(set: util.Set[java.lang.Integer]):Set[Int] = {
    var a: Set[Int] = Set[Int]()
    val b = set.asScala.toSet
    for(i <- b){
      a += i
    }
    a
  }
  def toScala2(constraint: JavaCheck): Array[Set[Int]]=>Array[Set[Int]] = {
    my_array =>{
      var a: util.ArrayList[util.Set[java.lang.Integer]] = new java.util.ArrayList[java.util.Set[java.lang.Integer]]();
      for(i <- my_array.indices){
        val set: util.Set[java.lang.Integer] = scToJ_Set(my_array(i))
        a.add(set)
      }
      val cons: util.ArrayList[util.Set[Integer]] = constraint.Constraint(a)
      val result = new Array[Set[Int]](my_array.length)
      for(i<- result.indices){
        val s: util.Set[Integer] = cons.get(i)
        result(i) = jToSc_Set(s)
      }
      result
    }
  }

  def toScala(constraint:Array[java.util.Set[Int]]=>Array[java.util.Set[Int]]): Array[Set[Int]] => Array[Set[Int]] ={
    my_array =>{
      var a : Array[java.util.Set[Int]]= new Array[java.util.Set[Int]](my_array.length)
      for(i <- my_array.indices){
        var set : java.util.Set[Int]= my_array(i).asJava
        a(i) = set
      }
      var cons = constraint(a)
      var result = new Array[Set[Int]](my_array.length)
      for(i <- my_array.indices){
        var set = cons(i).asScala.toSet
        result(i) = set
      }
      result
    }

  }
  def check_allDifferent(constraint:JavaCheck):Unit = {
    val scala_constraint = toScala2(constraint)
    check_AllDifferent(scala_constraint)
  }

  def check_allDifferent_for_Java(constraint:Array[java.util.Set[Int]] => Array[java.util.Set[Int]]):Unit = {
    val scala_constraint = toScala(constraint)
    check_AllDifferent(scala_constraint)
  }
  /*
   * This function check if the constraint passed in argument apply correctly an
   * allDifferent constraint with arc consistency.
   */
  def check_AllDifferent(constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    forAll(Gen.containerOfN[Array,Set[Int]](8,Generator)){ x =>
      x.length == 0 || check_AllDiff(x,constraint)
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
    val reduced_domains: Array[Set[Int]] = constraint_tested(variables)
    // Then we generate the domains that reduced_domains should have
    val true_reduced_domains: Array[Set[Int]] = cartesian_product(variables,AllDifferent1)

    //Finally, we compare the two. If they are not equals, the constraint is not correct.
    for(i<- reduced_domains.indices){
      if(!true_reduced_domains(i).equals(reduced_domains(i))){
        println("failed for: "+ variables.toList)
        println("you should have: "+ true_reduced_domains.toList)
        println("but you had "+ reduced_domains.toList)
        return false
      }
    }
    true
  }

  def main(args: Array[String]): Unit ={
    check_AllDifferent(AllDifferent)
  }

}