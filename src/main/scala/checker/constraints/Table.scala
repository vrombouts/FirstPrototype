package checker.constraints

import checker.{Generators, LimitCases, NoSolutionException}
import org.scalacheck.Prop.forAll

object Table extends checker.Checker {
  private var table:Set[Array[Int]] = Set.empty

  def printTable(table: Set[Array[Int]]):Unit = {
    println("With Table : ")
    for(x <- table){
      print("[ ")
      x.foreach(y=>print(" "+y))
      println("]")
    }
    println()
  }

  def checkAC(constraint: (Array[Set[Int]],Set[Array[Int]])=>Array[Set[Int]]): Unit= {
    forAll(Generators.table) { list =>
      val variables = list._1
      table = list._2

      if(variables.isEmpty || checkEmpty(variables) || table.isEmpty) true
      else if(!checkConstraint(variables.toArray, constraint(_, table))){
        printTable(table)
        false
      }else true
    }.check
    LimitCases.tableLimitCases.foreach{x =>
      table= x._2
      if(!checkConstraint(x._1, constraint(_,x._2)))
        printTable(x._2)}
  }

  def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    var solutions: Set[Array[Int]] = Set.empty
    table.foreach(solution => if(possibleWith(variables,solution)) solutions += solution)
    if(solutions.isEmpty) throw new NoSolutionException
    Constraint.toDomainsAC(solutions.toArray)
  }

  private def possibleWith(variables: Array[Set[Int]], solution: Array[Int]): Boolean = {
    for(i <- solution.indices){
      if(!variables(i).contains(solution(i))) return false
    }
    true
  }

}
