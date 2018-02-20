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

  @throws[NoSolutionException]
  private def cartesianProduct(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Array[Int]]={
    if(variables.length < 1) {throw new NoSolutionException}
    var stream:Stream[Array[Int]] = instantiateStream(variables(0))
    for(i<- 1 until variables.length){
      stream = cartesian(stream,variables(i)).filter(constraint)
    }
    val solutions: Array[Array[Int]] = stream.toArray
    solutions
  }

  @throws[NoSolutionException]
  def applyAC(variables:Array[Set[Int]],constraint:Array[Int]=>Boolean) : Array[Set[Int]] = {
    val sol = cartesianProduct(variables,constraint)
    if(sol.isEmpty) throw new NoSolutionException
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

  @throws[NoSolutionException]
  private def getIntervals(variables:Array[Set[Int]]) : Array[Interval]= {
   variables.map(x=>if(x.nonEmpty) new Interval(x) else throw new NoSolutionException)
  }

  private def intervalsToVariables(intervals: Array[Interval]) : Array[Set[Int]] = {
    intervals.map(x => x.domain)
  }

  @throws[NoSolutionException]
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

  @throws[NoSolutionException]
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
    //no solution if we remove the last element of the domain of a variable
    if(interval.domain.size==1) throw new NoSolutionException
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

  def addWithoutOverflow(sum:Int, value:Int):Int={
    if(value<0){
      if(Integer.MIN_VALUE-value>sum) Integer.MIN_VALUE
      else sum+value
    }else{
      if(Integer.MAX_VALUE-value<sum) Integer.MAX_VALUE
      else sum+value
    }
  }

  @throws[NoSolutionException]
  def sumBC(variables:Array[Set[Int]], constant:Int,operation:Int) : Array[Set[Int]] = {
    var changed:Boolean=true
    val cond: (Int, Int) => Boolean = Op.condition(operation,_,_,constant)
    while(changed){
      changed=false
      for(i <- variables.indices){
        if(variables(i).isEmpty)
          throw new NoSolutionException
        val min: Int= variables(i).min
        val max: Int= variables(i).max
        var sMin:Int= -min
        var sMax:Int= -max
        variables.foreach(x=>{
          sMin = addWithoutOverflow(sMin,x.min)
          sMax = addWithoutOverflow(sMax,x.max)})

        //Sum is BC so check min and max values
        if(cond(addWithoutOverflow(sMin,min), addWithoutOverflow(sMax,min))) {
          variables(i) = variables(i) - min
          if(variables(i).isEmpty) throw new NoSolutionException
          changed = true
        }
        if(cond(addWithoutOverflow(sMin,max), addWithoutOverflow(sMax,max))){
          variables(i) = variables(i) - max
          if(variables(i).isEmpty) throw new NoSolutionException
          changed = true
        }
      }
    }
    variables
  }

  def allDifferent(solution: Array[Int]):Boolean = {
    solution.toSet.size==solution.length
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

  /*
   * This constraint is used to test. It does absolutely nothing.
   */
  def dummyConstraint(x:Array[Set[Int]]): Array[Set[Int]] = x

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
