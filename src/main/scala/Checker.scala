
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
  def Generator_List_Of_Variables(): Gen[List[Variable]]={
    Gen.containerOfN[List,Variable](10,Generator_variable())
  }

  def check_AllDifferent(Body : List[Variable] => Unit): Unit = {
    forAll(Generator_List_Of_Variables()){x =>
      check_AllDifferent(x,Body)
    }.check
  }
  def check_AllDifferent(variables:List[Variable],Body:List[Variable] => Unit): Boolean ={
    val result = generate_solutions(variables, AllDifferent)
    println(result)
    println("#################################")
    Body(variables)
    println(variables)
    for (i <- result.indices) {
      if (!result(i).same_domain(variables(i))) {
        return false
      }
    }
    true
  }

 def main(args: Array[String]): Unit ={
   check_AllDifferent(AllDifferent)
  }

}