package checker.constraints

import checker._
import org.scalacheck.Prop.forAll

import scala.collection.mutable

object Element extends Checker{

  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = elementAC(variables)

  override def applyConstraint(b:BranchOp):Array[Set[Int]] = null

  def checkAC(constraint: Array[Set[Int]] => Array[Set[Int]]) : Unit={
    val check: (Array[Set[Int]]) => Boolean = checkConstraint(_,constraint)
    forAll(Generators.element){ x =>
      val X :Array[Set[Int]] = x._1.toArray
      val i:Set[Int] = x._2
      val v:Set[Int] = x._3
      val variables:Array[Set[Int]] = X ++ Array(i,v)
      checkEmpty(variables.toList) || X.length <= 0 || i.size <= 0 || v.size <=0 || check(variables) //x.length<2
    }.check
    LimitCases.elementLimitCases.foreach(limitCase => {check(limitCase)})
  }

  override def printer(initial:Array[Set[Int]],trueReduced:Array[Set[Int]],reduced:Array[Set[Int]],error:Boolean,ourError:Boolean):Unit = {
    val init = initial.dropRight(2).toList
    val initI = initial(initial.length-2)
    val initV = initial(initial.length-1)
    val tr = trueReduced.dropRight(2).toList
    val trI = trueReduced(trueReduced.length-2)
    val trV = trueReduced(trueReduced.length-1)
    val r = reduced.dropRight(2).toList
    val rI = reduced(reduced.length-2)
    val rV = reduced(reduced.length-1)
    if(error && !ourError) {
      println("failed with X domains: " + init + "\n i domain: " + initI + "\n v domain: " +initV)
      println("you should have: " + tr+ "\n i domain: " + trI + "\n v domain: " +trV)
      println("but you returned an exception")
    }else if(!error && ourError){
      println("failed with X domains: " + init+ "\n i domain: " + initI + "\n v domain: " +initV)
      println("you should not have any solutions")
      println("but you had: " + r+ "\n i domain: " + rI + "\n v domain: " +rV)
    }else if(!error && !ourError){
      println("failed for: " + init + "\n i domain: " + initI + "\n v domain: " +initV)
      println("you should have: " + tr + "\n i domain: " + trI + "\n v domain: " +trV)
      println("but you had: " + r + "\n i domain: " + rI + "\n v domain: " +rV)
    }
  }

  /*
  * element constraint : element(X,i,v). Here, the i and v are put at the end of the vector for convenient purposes
  * The variables x in argument represents the set of variables and x(x.length-1) = v while x(x.length-2) = i
  */
  @throws[NoSolutionException]
  def elementAC(x:Array[Set[Int]]): Array[Set[Int]] ={
    val X:Array[Set[Int]] = x.dropRight(2)
    val i:Set[Int] = x(x.length-2)
    val v:Set[Int]=x(x.length-1)

    var reducedDomainv:mutable.Set[Int]=mutable.Set()
    var reducedDomaini:mutable.Set[Int]=mutable.Set()

    for(value <- v){
      for(index <- i){
        if(index >= 0 && index < X.length && X(index).contains(value)){
          reducedDomainv += value
          reducedDomaini += index
        }
      }
    }

    if(reducedDomainv.isEmpty || reducedDomaini.isEmpty)
      throw new NoSolutionException

    if(reducedDomaini.size == 1) {
      val set:mutable.Set[Int]=reducedDomainv.clone()
      X(reducedDomaini.head) = set.toSet
    }

    X ++ Array(reducedDomaini.toSet,reducedDomainv.toSet)
  }

  /*
   * element(x,i,v) constraint
   * Important to note that the solution array contains the variables x followed by i and v
   */
  def elementConstraint(solution:Array[Int]):Boolean = {
    val X:Array[Int]=solution.dropRight(2)
    val i:Int=solution(solution.length-2)
    val v:Int=solution(solution.length-1)
    if(i>X.length) return false
    X(i) == v
  }

}
