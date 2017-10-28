package checker

import scala.collection.immutable.Stream.cons

/**
  * Created by valentin on 13.10.17.
  */

object Constraints {

  def cartesian(sol:Stream[Array[Int]],variable: Set[Int]): Stream[Array[Int]] = {
    var new_stream : Stream[Array[Int]] = Stream.empty
    for(s<-sol) {
      for (value <- variable) {
        val array = Array.concat(s,Array(value))
        val strm: Stream[Array[Int]] = cons(array,Stream.empty)
        new_stream = new_stream.append(strm)
      }
    }
    new_stream
  }

  def instantiate_stream(variable:Set[Int]): Stream[Array[Int]]={
    var stream = Stream.empty[Array[Int]]
    for(i<-variable){
      stream = stream.append(cons(Array(i),Stream.empty))
    }
    stream
  }

  def cartesian_product(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Set[Int]]={
    // TO DO : generate an error msg and add a try-catch statement
    if(variables.length < 1) {return Array[Set[Int]]()}
    var stream:Stream[Array[Int]] = instantiate_stream(variables(0))
    for(i<- 1 until variables.length){
      stream = cartesian(stream,variables(i)).filter(constraint)
    }
    val solutions: Array[Array[Int]] = stream.toArray
    toDomains(solutions)
  }

  def toDomains(solutions: Array[Array[Int]]): Array[Set[Int]] ={
    if(solutions.length < 1) return Array[Set[Int]]()
    val variables: Array[Set[Int]] = new Array[Set[Int]](solutions(0).length)
    for(i<-variables.indices){
      var domain = Set.empty[Int]
      for(j<-solutions.indices){
        domain +=  solutions(j)(i)
      }
      variables(i) = collection.mutable.SortedSet(domain.toList:_*).toSet
    }
    variables
  }

  def AllDifferent1(solution: Array[Int]):Boolean = {
    val set = solution.toSet
    if(set.size!=solution.length) return false
    true
  }

  def convert(x:Array[Set[Int]]):Array[Variable]={
    var res = Array[Variable]()
    for(i <- x.indices){
      res :+= new Variable(x(i), i)
    }
    res
  }

  def convert(x:Array[Variable]):Array[Set[Int]]={
    var res = Array[Set[Int]]()
    for(i <- x.indices){
      res :+= x(i).domain.toSet
    }
    res
  }



  def AllDifferent(x:Array[Set[Int]]): Array[Set[Int]] = {
    var change=true
    var emptySet = false
    val variables = convert(x)
    while(change && !emptySet){
      val y: Array[Variable] = variables.sortWith(_ compare_domain _)
      change=false
      var halles: List[Halles] = List()
      var i = 0
      while(!change && i<y.length){
        var found = false
        for(hall<- halles){
          if (hall.possible_add(y(i))){
            found = true
            hall.add(y(i))
            change = hall.propagate(y)
          }
        }
        if(!found){
          val hall = new Halles(y(i).domain.toSet,y(i).id)
          halles = hall::halles
          change = hall.propagate(y)
        }
        i = i+1
      }
      emptySet = CheckEmptySet(y)
    }
    convert(variables)
  }

  def CheckEmptySet(x: Array[Variable]): Boolean = {
    val y = x.filter(v => v.domain.isEmpty)
    if(y.nonEmpty){
      x.foreach(v => v.domain = v.domain.empty)
      return true
    }
    false
  }


  def AllDifferent(x:Array[Int],index:Int) : Boolean = {
    for(i<- 0 until index) if(x(index)==x(i)) return false
    true
  }

  def compareSets(s1:Set[Int], s2:Set[Int]): Boolean ={
    if(s1.size != s2.size) return false
    val sorted1 = collection.mutable.SortedSet(s1.toList: _*)
    val sorted2 = collection.mutable.SortedSet(s2.toList: _*)
    for(i <- 1 to s1.size){
      if(sorted1(i) != sorted2(i)) return false
    }
    true
  }

    def generate_solutions(x:List[Variable], index:Int, current_sol:Array[Int], result:List[Variable],constraint:(Array[Int],Int)=>Boolean): Unit ={
      if(x.length <= index) {
        union(current_sol, result)
      }else {
        for (set <- x(index).domain) {
          current_sol(index) = set
          if(constraint(current_sol,index)){
            generate_solutions(x, index + 1, current_sol, result,constraint)
          }
        }
      }
    }

    def union(l:Array[Int], v:List[Variable]): Unit={
      for(i <- l.indices){
        v(i).domain += l(i)
      }
    }

    def generate_solutions(x:List[Variable], constraint:(Array[Int],Int)=>Boolean): List[Variable] ={
      val sorted = x.sortWith(_ compare_domain _)
      val result: List[Variable] = sorted.map(v => new Variable(Set(),v.id))
      val current_sol: Array[Int] = Array.fill[Int](sorted.length)(0)
      generate_solutions(sorted,0,current_sol,result,constraint)
      result.sortWith(_ compare_id _)
    }





  def main(args: Array[String]) {
    //val x = Checker.Generator_List_Of_Variables(10).sample
    val variables = Array(Set(0,1,2))
    val result: Array[Set[Int]] = cartesian_product(variables,AllDifferent1)
    println("result: [")
    for(i<-result.indices){
      println(result(i))
    }
    print("]")
  }


}
