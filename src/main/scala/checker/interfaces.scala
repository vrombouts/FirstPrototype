package checker

import checker.Checker._
import scala.collection.JavaConverters._


trait JCpC {
    @throws[Exception]
    def constraint(tab: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]
}

abstract class JCpChecker extends JCpC
  with AllDifferentConstraint with SumConstraint {

  implicit def int2IntegerSet(x: java.util.Set[Int]): java.util.Set[Integer] ={
    val result : java.util.Set[Integer] = new java.util.HashSet[Integer]()
    val iterator = x.iterator()
    while(iterator.hasNext){
      val a: java.lang.Integer = new Integer(iterator.next())
      result.add(a)
    }
    result
  }
  implicit def Integer2intSet(x: Set[Integer]): Set[Int] ={
    var result : Set[Int] = Set[Int]()
    for (elem <- x){
      val a: Int = elem.asInstanceOf[Int]
      result += a
    }
    result
  }
  def scalaConstraint: Array[Set[Int]] => Array[Set[Int]] ={
    myArray =>{
      val a : Array[java.util.Set[Integer]]= new Array[java.util.Set[Integer]](myArray.length)
      for(i <- myArray.indices){
        val set = myArray(i).asJava
        a(i) = set
      }
      val cons = constraint(a)
      val result = new Array[Set[Int]](myArray.length)
      for(i <- myArray.indices){
        val set = cons(i).asScala.toSet
        result(i) = set
      }
      result
    }

  }
  def constraint(vars: Array[Set[Int]]): Array[Set[Int]] = scalaConstraint(vars)
}

trait ScCpChecker extends AllDifferentConstraint with SumConstraint{}

trait SumConstraint extends Constraint{
  def checkSumEqual(constant:Int):Unit =              checkSumConstraint(constant, Op.equal)
  def checkSumDifferent(constant:Int):Unit =          checkSumConstraint(constant, Op.different)
  def checkSumLesserThan(constant:Int):Unit =         checkSumConstraint(constant, Op.lesserThan)
  def checkSumLesserThanOrEqual(constant:Int):Unit =  checkSumConstraint(constant, Op.lesserThanOrEqual)
  def checkSumGreaterThan(constant:Int):Unit =        checkSumConstraint(constant, Op.greaterThan)
  def checkSumGreaterThanOrEqual(constant:Int):Unit = checkSumConstraint(constant, Op.greaterThanOrEqual)

  def checkSumConstraint(constant:Int, operation:Int):Unit = {
    checkSummation(constraint,constant,operation)
  }
}

trait AllDifferentConstraint extends Constraint{
  def checkAllDifferentConstraintAC():Unit = {
    checkAllDifferentAC(constraint)
  }
  def checkAllDifferentConstraintBC():Unit = {
    checkAllDifferentBC(constraint)
  }
}

trait Constraint {
  def constraint(vars: Array[Set[Int]]):Array[Set[Int]]
}