
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import Constraints._

object Checker {
  var j = 0
  val Generator: Gen[Set[Int]] =  Gen.containerOfN[Set,Int](3,Gen.choose(0,20))
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

  /*
   * This function check if the constraint passed in argument apply correctly an
   * allDifferent constraint with arc consistency.
   */
  def check_AllDifferent(constraint:Array[Set[Int]]=>Array[Set[Int]]): Boolean = {
    /*forAll(Gen.containerOf[Array,Set[Int]](Generator)){ x =>
      check_AllDifferent(x,constraint)
    }.check*/

    val test1 = Array(Set(0,1,2),Set(0))
    check_AllDifferent(test1,constraint)
    val test2 = Array(Set(0,1),Set(0,1),Set(0,1,2))
    check_AllDifferent(test2,constraint)
    val test3 = Array(Set(0),Set(1),Set(2))
    check_AllDifferent(test3,constraint)
    val test4 = Array(Set(0,1),Set(1,2),Set(2,3),Set(3,4),Set(4,5),Set(2,4))
    check_AllDifferent(test4,constraint)
    val test5 = Array(Set(0,1,2))
    check_AllDifferent(test5,constraint)

  }

  def check_AllDifferent(variables:Array[Set[Int]],constraint_tested:Array[Set[Int]]=>Array[Set[Int]]): Boolean ={
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