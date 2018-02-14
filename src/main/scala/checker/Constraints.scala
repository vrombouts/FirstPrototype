package checker

import scala.collection.immutable.Stream.cons

object Constraints {

  private def cartesian(sol:Stream[Array[Int]],variable: Set[Int]): Stream[Array[Int]] = {
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

  private def instantiateStream(variable:Set[Int]): Stream[Array[Int]]={
    var stream = Stream.empty[Array[Int]]
    for(i<-variable){
      stream = stream.append(cons(Array(i),Stream.empty))
    }
    stream
  }

  private def cartesianProduct(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Array[Int]]={
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

  private def toDomainsAC(solutions: Array[Array[Int]]): Array[Set[Int]] ={
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

  private def getIntervals(variables:Array[Set[Int]]) : Array[Interval]= {
   variables.map(x=> new Interval(x))
  }

  private def intervalsToVariables(intervals: Array[Interval]) : Array[Set[Int]] = {
    intervals.map(x => x.domain)
  }

  def applyBC(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Set[Int]] = {
    val intervals = getIntervals(variables)
    var changed:Boolean = true
    while(changed) {
      changed = false
      for (i <- variables.indices) {
        val modif:Boolean = cartesianBC(intervals, constraint, i, minOrMax = true)
        val other_modif: Boolean = cartesianBC(intervals, constraint, i, minOrMax = false)
        if(modif || other_modif) changed=true
      }
    }
    intervalsToVariables(intervals)
  }

  private def reinitialize(intervals:Array[Interval]) : Unit={
    intervals.foreach(x => x.resetPos())
  }

  private def findingAcceptingValue(sol: Array[Int], interval: Interval, constraint: Array[Int]=>Boolean): Boolean = {
    if(constraint(sol)) true
    else if(interval.posInInterval){
      sol(sol.length-1) = interval.position
      interval.incrementPos()
      findingAcceptingValue(sol,interval, constraint)
    }else{
      interval.resetPos()
      false
    }
  }

  private def cartesianBC(intervals:Array[Interval],constraint:Array[Int]=>Boolean, id:Int, minOrMax:Boolean): Boolean ={
    reinitialize(intervals)
    val interval:Interval = intervals(id)
    var sol:Array[Int] = Array(interval.giveValue(minOrMax))
    if(intervals.length==1){
      if(constraint(sol))
        return false
    }
    var i:Int=0
    while(i<intervals.length && sol.nonEmpty) {
      if (i == id) i = i + 1
      val currentInter = intervals(i)
      if (currentInter.posInInterval) {
        sol = sol :+ currentInter.position
        currentInter.incrementPos()
        if (!findingAcceptingValue(sol, currentInter, constraint)) {
          //backtrack
          sol = sol.dropRight(2)
          i = if (i == id + 1) i - 3 else i - 2
        } else if (sol.length == intervals.length)
          //one element of the cartesian product respect the constraint.
          //Therefore, no update of the interval's min/max is required
          return false
      } else {
        //backtrack
        currentInter.resetPos()
        sol = sol.dropRight(1)
        i = if (i == id + 1) i - 3 else i - 2
      }
      i = i + 1
    }
    interval.update(minOrMax)
    true

  }


  def sum(constant:Int,operation:Int,nbVar:Int): Array[Int] => Boolean = sum(constant,operation,nbVar,_)
  def sum(constant:Int,operation:Int,nbVar: Int, solution: Array[Int]):Boolean = {
    if(solution.length<nbVar-1) return true
    var sum:Int = 0
    solution.foreach(x => sum += x)
    Op.respectOp(operation,sum,constant)
  }

  @throws[Exception]
  def sumBC(variables:Array[Set[Int]], constant:Int,operation:Int) : Array[Set[Int]] = {
    var changed:Boolean=true
    var result=variables.clone()
    val cond: (Int, Int, Int) => Boolean = Op.condition(operation,_,_,constant,_)
    while(changed){
      changed=false
      for(i <- result.indices){
        val min: Int= result(i).min
        val max: Int= result(i).max
        var sMin:Int= -min
        var sMax:Int= -max
        result.foreach(x=>{sMin+=x.min ; sMax+=x.max})
        //Sum is BC so check min and max values
        if(cond(sMin, sMax, min)) {
          result(i) = result(i) - min
          changed = true
        }
        if(cond(sMin, sMax, max)){
          result(i) = result(i) - max
          changed = true
        }
        if(result(i).isEmpty)
          throw new Exception
      }
    }
    result
  }

  def allDifferent(solution: Array[Int]):Boolean = {
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
    val result: Array[Set[Int]] = applyAC(variables,allDifferent)
    println("result: [")
    for(i<-result.indices){
      println(result(i))
    }
    print("]")
  }


}
