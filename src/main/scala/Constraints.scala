/**
  * Created by valentin on 13.10.17.
  */

object Constraints {

  def AllDifferent(x:List[Variable]):Unit = {
    var change=true
    var emptySet = false
    while(change && !emptySet){
      val y = x.sortWith(_ compare_domain _)
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
  }

  def CheckEmptySet(x: List[Variable]): Boolean = {
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
    val x = Checker.Generator_List_Of_Variables(10).sample
    var variables:List[Variable]=null
    x match {
      case Some(i) => variables=i
      case None => variables=null
    }
    val result= generate_solutions(variables,AllDifferent)
    println(result)
    println("###############")
    println(variables)
  }


}
