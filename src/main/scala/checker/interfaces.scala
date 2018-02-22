package checker

import scala.collection.JavaConverters._
import java.util.function.Function

class JCpChecker {
  def checkAC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    Checker.checkAC(filtering,checker)
  }
  def checkBC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    Checker.checkBC(filtering,checker)
  }
  def checkAllDifferentAC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit={
    Checker.checkAllDifferent(isAC = true, filtering)
  }
  def checkAllDifferentBC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit={
    Checker.checkAllDifferent(isAC = false, filtering)
  }
  def checkSumEQ(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Checker.checkSummation(filtering,Op.equal)
  }
  def checkSumNE(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Checker.checkSummation(filtering,Op.different)
  }
  def checkSumLT(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Checker.checkSummation(filtering,Op.lesserThan)
  }
  def checkSumLE(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Checker.checkSummation(filtering,Op.lesserThanOrEqual)
  }
  def checkSumGT(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Checker.checkSummation(filtering,Op.greaterThan)
  }
  def checkSumGE(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Checker.checkSummation(filtering,Op.greaterThanOrEqual)
  }

  implicit private def filterToScalaFunction(fun: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]): Array[Set[Int]] => Array[Set[Int]] = {
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
  implicit private def checkerToScalaFunction(fun: Function[Array[Integer],java.lang.Boolean]): Array[Int] => Boolean = {
    myArray =>{
      val ar: Array[Integer] = myArray.map(x => new Integer(x))
      val bool: Boolean = fun.apply(ar)
      bool
    }
  }
  implicit private def int2IntegerSet(x: java.util.Set[Int]): java.util.Set[Integer] ={
    val result : java.util.Set[Integer] = new java.util.HashSet[Integer]()
    val iterator = x.iterator()
    while(iterator.hasNext){
      val a: java.lang.Integer = new Integer(iterator.next())
      result.add(a)
    }
    result
  }
  implicit private def Integer2intSet(x: Set[Integer]): Set[Int] ={
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
  def checkSumEQ(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Checker.checkSummation(filteringTested, Op.equal)
  }
  def checkSumNE(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Checker.checkSummation(filteringTested, Op.different)
  }
  def checkSumLT(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Checker.checkSummation(filteringTested, Op.lesserThan)
  }
  def checkSumLE(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Checker.checkSummation(filteringTested, Op.lesserThanOrEqual)
  }
  def checkSumGT(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Checker.checkSummation(filteringTested, Op.greaterThan)
  }
  def checkSumGE(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Checker.checkSummation(filteringTested, Op.greaterThanOrEqual)
  }
}
trait Constraint {
  def constraint(vars: Array[Set[Int]]):Array[Set[Int]]
  //def checkConstraint(solution: Array[Int]): Boolean //= throw new checkUnimplementedException()
}