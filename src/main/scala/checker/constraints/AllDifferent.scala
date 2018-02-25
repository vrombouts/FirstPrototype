package checker.constraints
import checker.{Checker, Halles, LimitCases, Variable}
import checker.constraints.Constraint
object AllDifferent extends Checker{

  private var isAC:Boolean = true
  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if(isAC) Constraint.applyAC(variables,allDifferent)
    else Constraint.applyBC(variables,allDifferent)
  }
  /*
  * This function checks if the constraint passed in argument apply correctly an
  * allDifferent constraint with arc consistency.
  */
  def checkAllDifferent(isAc: Boolean, constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    isAC=isAc
    if(isAc) {
      Constraint.checkAC(constraint,allDifferent)
    }
    else Constraint.checkBC(constraint,allDifferent)
    val checkAllDiff: Array[Set[Int]] => Boolean = checkConstraint(_,constraint)
    LimitCases.allDifferentLimitCases.foreach(x => checkAllDiff(x))
    println("All tests executed.")
  }

  def allDifferent(solution: Array[Int]):Boolean = {
    solution.toSet.size==solution.length
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

}
