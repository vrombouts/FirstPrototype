package checker

import scala.collection.immutable.Stream.cons

object Constraints {

  def cartesian(sol:Stream[Array[Int]],variable: Set[Int]): Stream[Array[Int]] = {
    var newStream : Stream[Array[Int]] = Stream.empty
    for(s<-sol) {
      for (value <- variable) {
        val array = Array.concat(s,Array(value))
        val strm: Stream[Array[Int]] = cons(array,Stream.empty)
        newStream = newStream.append(strm)
      }
    }
    newStream
  }

  def instantiateStream(variable:Set[Int]): Stream[Array[Int]]={
    var stream = Stream.empty[Array[Int]]
    for(i<-variable){
      stream = stream.append(cons(Array(i),Stream.empty))
    }
    stream
  }

  def cartesianProduct(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Array[Int]]={
    // TO DO : generate an error msg and add a try-catch statement
    if(variables.length < 1) {return Array[Array[Int]]()}
    var stream:Stream[Array[Int]] = instantiateStream(variables(0))
    for(i<- 1 until variables.length){
      stream = cartesian(stream,variables(i)).filter(constraint)
    }
    val solutions: Array[Array[Int]] = stream.toArray
    solutions
  }

  def applyAC(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Set[Int]] = {
    val sol = cartesianProduct(variables,constraint)
    val result=toDomainsAC(sol)
    result
  }

  def getIntervals(variables:Array[Set[Int]]) : Array[Interval]= {
    var inters : Array[Interval] = new Array[Interval](variables.length)
    for(i <- variables.indices){
      inters(i) = new Interval(variables(i))
    }
    return inters
  }

  def intervalsToVariables(intervals: Array[Interval]) : Array[Set[Int]] = {
    var result : Array[Set[Int]] = Array.fill[Set[Int]](intervals.length)(Set.empty)
    for(i <- intervals.indices){
      result(i) = intervals(i).domain
    }
    return result
  }

  def applyBC(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Set[Int]] = {
    var intervals = getIntervals(variables)
    var changed:Boolean = true
    while(changed) {
      changed = false
      for (i <- variables.indices) {
        var modif:Boolean = cartesianBC(intervals, constraint, i, true)
        var other_modif: Boolean = cartesianBC(intervals, constraint, i, false)
        if(modif || other_modif) changed=true
      }
    }
    return intervalsToVariables(intervals)
  }

  def toDomainsAC(solutions: Array[Array[Int]]): Array[Set[Int]] ={
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


  def toDomainsBC(solutions: Array[Array[Int]],variables: Array[Set[Int]]):Array[Set[Int]] ={
    val ac = toDomainsAC(solutions)
    var result : Array[Set[Int]] = variables.clone()
    for(i <- variables.indices){
      val min = ac(i).min
      val max = ac(i).max
      result(i) = result(i).filter(_ >= min)
      result(i) = result(i).filter(_ <= max)
    }
    result
  }

  def reinitialize(variables:Array[Interval]) : Unit={
    for(i <- variables.indices){
      variables(i).pos=variables(i).min
    }
  }

  def cartesianBC(variables:Array[Interval],constraint:Array[Int]=>Boolean, id:Int, minOrMax:Boolean): Boolean ={
    var inter:Interval = variables(id)
    var sol:Array[Int] = Array(inter.giveValue(minOrMax))
    var i:Int=0
    reinitialize(variables)
    while(i<variables.length && !sol.isEmpty){
      if(i!=id && variables(i).pos <= variables(i).max) {
        sol = sol :+ variables(i).pos
        variables(i).pos = variables(i).pos + 1
        var condition: Boolean = true
        while (!constraint(sol) && condition) {
          if (variables(i).pos > variables(i).max) {
            variables(i).pos = variables(i).min
            condition = false
            sol = sol.dropRight(2)
            i = i - 2
            if(i==id-1)
              i=i-1
          }
          else {
            sol(sol.length-1) = variables(i).pos
            variables(i).pos = variables(i).pos + 1
          }
         /* println("#########")
          for(j <- sol.indices){
            print(sol(j) + " ")
          }
          println("#########")*/
        }
        /*for(j <- sol.indices){
          print(sol(j) + " ")
        }
        println()*/
        if (sol.length == variables.length && constraint(sol)) {
          return false
        }
      }
      else if(i!=id){
        variables(i).pos=variables(i).min
        sol=sol.dropRight(1)
        i=i-2
        if(i==id-1)
          i=i-1
      }
      i=i+1
    }
    inter.update(minOrMax)
    return true

  }



  def allDifferent1(solution: Array[Int]):Boolean = {
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



  def allDifferent(x:Array[Set[Int]]): Array[Set[Int]] = {
    var change=true
    var emptySet = false
    val variables = convert(x)
    while(change && !emptySet){
      val y: Array[Variable] = variables.sortWith(_ compareDomain _)
      change=false
      var halles: List[Halles] = List()
      var i = 0
      while(!change && i<y.length){
        var found = false
        for(hall<- halles){
          if (hall.possibleAdd(y(i))){
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
      emptySet = checkEmptySet(y)
    }
    convert(variables)
  }

  def checkEmptySet(x: Array[Variable]): Boolean = {
    val y = x.filter(v => v.domain.isEmpty)
    if(y.nonEmpty){
      x.foreach(v => v.domain = v.domain.empty)
      return true
    }
    false
  }


  def allDifferent(x:Array[Int],index:Int) : Boolean = {
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

    def generateSol(x:List[Variable], index:Int, currentSol:Array[Int], result:List[Variable],constraint:(Array[Int],Int)=>Boolean): Unit ={
      if(x.length <= index) {
        union(currentSol, result)
      }else {
        for (set <- x(index).domain) {
          currentSol(index) = set
          if(constraint(currentSol,index)){
            generateSol(x, index + 1, currentSol, result,constraint)
          }
        }
      }
    }

    def union(l:Array[Int], v:List[Variable]): Unit={
      for(i <- l.indices){
        v(i).domain += l(i)
      }
    }

    def generateSolutions(x:List[Variable], constraint:(Array[Int],Int)=>Boolean): List[Variable] ={
      val sorted = x.sortWith(_ compareDomain _)
      val result: List[Variable] = sorted.map(v => new Variable(Set(),v.id))
      val currentSol: Array[Int] = Array.fill[Int](sorted.length)(0)
      generateSol(sorted,0,currentSol,result,constraint)
      result.sortWith(_ compareID _)
    }





  def main(args: Array[String]) {
    //val x = Checker.Generator_List_Of_Variables(10).sample
    val variables = Array(Set(0,1,2))
    val result: Array[Set[Int]] = applyAC(variables,allDifferent1)
    println("result: [")
    for(i<-result.indices){
      println(result(i))
    }
    print("]")
  }


}
