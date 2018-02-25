package checker.constraints

import checker.{Checker, Generators, Interval, NoSolutionException}
import org.scalacheck.Prop.forAll

import scala.collection.immutable.Stream.cons
import scala.collection.mutable


object Constraint extends Checker{

  private var isAC:Boolean=true
  private var checkFunction:Array[Int] =>Boolean = null

  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if(isAC) applyAC(variables,checkFunction)
    else applyBC(variables,checkFunction)
  }
  def checkAC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    checkFunction = checker
    isAC=true
    forAll(Generators.basic){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringTested)
    }.check
    //TODO: add simple case limit possible for all constraints
  }

  def checkBC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    checkFunction = checker
    isAC=false
    forAll(Generators.basic){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringTested)
    }.check
    //TODO: add simple case limit possible for all constraints
  }

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

  def solutions(variables:Array[Set[Int]]):Array[Array[Int]] = {
    val currentSol: Array[Int] = Array.fill(variables.length)(0)
    val result: mutable.Set[Array[Int]]= mutable.Set()
    setIthVariable(variables,0,currentSol,result)
    result.toArray
  }
  private def setIthVariable(variables:Array[Set[Int]],index : Int, currentSol:Array[Int], result:mutable.Set[Array[Int]]):Unit={
    for(i <- variables(index)){
      currentSol(index) = i
      if(index==variables.length-1){
        result += currentSol.clone()
      }else{
        setIthVariable(variables,index+1,currentSol,result)
      }
    }
  }
  def applyACWithoutPruning(variables:Array[Set[Int]], constraint: Array[Int]=>Boolean) : Array[Set[Int]]={
    val sols: Array[Array[Int]] = solutions(variables).filter(x => constraint(x))
    toDomainsAC(sols)
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





  /*
   * This constraint is used to test. It does absolutely nothing.
   */
  def dummyConstraint(x:Array[Set[Int]]): Array[Set[Int]] = x

}
