package checker

import checker.Checker._
import scala.collection.JavaConverters._
import java.util.function.Function

class JCpChecker {
  def checkAC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    val scalaFilter = filterToScalaFunction(filtering)
    val scalaChecker= checkerToScalaFunction(checker)
    Checker.checkAC(scalaFilter,scalaChecker)
  }
  def checkBC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    val scalaFilter = filterToScalaFunction(filtering)
    val scalaChecker= checkerToScalaFunction(checker)
    Checker.checkBC(scalaFilter,scalaChecker)
  }
  def checkAllDifferentAC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit={
    val scalaFilter = filterToScalaFunction(filtering)
    Checker.checkAllDifferent(isAC = true, scalaFilter)
  }
  def checkAllDifferentBC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit={
    val scalaFilter = filterToScalaFunction(filtering)
    Checker.checkAllDifferent(isAC = false, scalaFilter)
  }

  private def filterToScalaFunction(fun: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):
  Array[Set[Int]] => Array[Set[Int]] = {
    myArray =>{
      val a : Array[java.util.Set[Integer]]= new Array[java.util.Set[Integer]](myArray.length)
      for(i <- myArray.indices){
        val set = myArray(i).asJava
        a(i) = set
      }
      val cons = fun.apply(a)
      val result = new Array[Set[Int]](myArray.length)
      for(i <- myArray.indices){
        val set = cons(i).asScala.toSet
        result(i) = set
      }
      result
    }
  }
  private def checkerToScalaFunction(fun: Function[Array[Integer],java.lang.Boolean]):
  Array[Int] => Boolean = {
    myArray =>{
      val ar: Array[Integer] = myArray.map(x => new Integer(x))
      val bool: Boolean = fun.apply(ar)
      bool
    }
  }
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
}
object ScCpChecker{
  def checkAC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    Checker.checkAC(filteringTested, checker)
  }
  def checkBC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    Checker.checkBC(filteringTested, checker)
  }
  def checkAllDifferentAC(filteringTested: Array[Set[Int]] => Array[Set[Int]]) : Unit = {
    Checker.checkAllDifferent(isAC = true, filteringTested)
  }
  def checkAllDifferentBC(filteringTested: Array[Set[Int]] => Array[Set[Int]]) : Unit = {
    Checker.checkAllDifferent(isAC = false, filteringTested)
  }
}

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


trait Constraint {
  def constraint(vars: Array[Set[Int]]):Array[Set[Int]]
  //def checkConstraint(solution: Array[Int]): Boolean //= throw new checkUnimplementedException()
}