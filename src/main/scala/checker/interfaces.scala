package checker

import scala.collection.JavaConverters._
import java.util.function.{BiFunction, Function}

import checker.constraints._

class JCpChecker {
  def checkAC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    Constraint.checkAC(filtering,checker)
  }
  def checkBC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    Constraint.checkBC(filtering,checker)
  }
  def checkAC(init: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              filtering : Function[BranchOp,Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    Constraint.checkAC(init, filtering,checker)
  }
  def checkBC(init: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]],
              filtering : Function[BranchOp,Array[java.util.Set[Integer]]],
              checker : Function[Array[Integer],java.lang.Boolean]): Unit ={
    Constraint.checkBC(init, filtering,checker)
  }
  def checkAllDifferentAC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit={
    AllDifferent.checkAC(filtering)
  }
  def checkAllDifferentBC(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit={
    AllDifferent.checkBC(filtering)
  }
  def checkSumEQ(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Sum.checkBC(filtering,Op.equal)
  }
  def checkSumNE(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Sum.checkBC(filtering,Op.different)
  }
  def checkSumLT(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Sum.checkBC(filtering,Op.lesserThan)
  }
  def checkSumLE(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Sum.checkBC(filtering,Op.lesserThanOrEqual)
  }
  def checkSumGT(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Sum.checkBC(filtering,Op.greaterThan)
  }
  def checkSumGE(filtering: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Sum.checkBC(filtering,Op.greaterThanOrEqual)
  }
  def checkElementAC(filteringTested: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]) : Unit={
    Element.checkAC(filteringTested)
  }
  def checkTableAC(tableFiltering: BiFunction[Array[java.util.Set[Integer]],java.util.Set[Array[Integer]],Array[java.util.Set[Integer]]]):Unit = {
    Table.checkAC(tableFiltering)
  }

  implicit private def tableFilterToScalaFunction(fun: BiFunction[Array[java.util.Set[Integer]],java.util.Set[Array[Integer]],Array[java.util.Set[Integer]]])
  : (Array[Set[Int]],Set[Array[Int]]) => Array[Set[Int]] = {
    (myArray,myTable) =>{
      val javaArray : Array[java.util.Set[Integer]]= new Array[java.util.Set[Integer]](myArray.length)
      for(i <- myArray.indices){
        val set = myArray(i).asJava
        javaArray(i) = set
      }
      val javaTable : java.util.Set[Array[Integer]] = new java.util.HashSet[Array[Integer]]()
      for(tableElement<- myTable){
        val javaTableElement:Array[Integer] = new Array[Integer](tableElement.length)
        for(i<- tableElement.indices){
          javaTableElement(i)=tableElement(i)
        }
        javaTable.add(javaTableElement)
      }
      val cons = fun.apply(javaArray,javaTable)
      val result = new Array[Set[Int]](myArray.length)
      for(i <- myArray.indices){
        val set = cons(i).asScala.toSet
        result(i) = set
      }
      result
    }
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
  implicit private def branchToScalaFunction(fun: Function[BranchOp,Array[java.util.Set[Integer]]]): BranchOp => Array[Set[Int]] = {
    branchOp: BranchOp => {
      val cons = fun.apply(branchOp)
      var result:Array[Set[Int]] = new Array[Set[Int]](cons.length)
      for(i <- cons.indices){
        val set = cons(i).asScala.toSet
        result(i) =  set
      }
      result
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
    Constraint.checkAC(filteringTested, checker)
  }
  def checkBC(filteringTested: Array[Set[Int]]=>Array[Set[Int]], checker:Array[Int]=>Boolean): Unit = {
    Constraint.checkBC(filteringTested, checker)
  }
  def checkAllDifferentAC(filteringTested: Array[Set[Int]] => Array[Set[Int]]) : Unit = {
    AllDifferent.checkAC( filteringTested)
  }
  def checkAllDifferentBC(filteringTested: Array[Set[Int]] => Array[Set[Int]]) : Unit = {
    AllDifferent.checkBC( filteringTested)
  }
  def checkSumEQ(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Sum.checkBC(filteringTested, Op.equal)
  }
  def checkSumNE(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Sum.checkBC(filteringTested, Op.different)
  }
  def checkSumLT(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Sum.checkBC(filteringTested, Op.lesserThan)
  }
  def checkSumLE(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Sum.checkBC(filteringTested, Op.lesserThanOrEqual)
  }
  def checkSumGT(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Sum.checkBC(filteringTested, Op.greaterThan)
  }
  def checkSumGE(filteringTested: Array[Set[Int]]=> Array[Set[Int]]) : Unit = {
    Sum.checkBC(filteringTested, Op.greaterThanOrEqual)
  }
  def checkElementAC(filteringTested: Array[Set[Int]] => Array[Set[Int]]) : Unit={
    Element.checkAC(filteringTested)
  }
  def checkTableAC(filteringWithTableTested: (Array[Set[Int]],Set[Array[Int]]) => Array[Set[Int]]):Unit={
    Table.checkAC(filteringWithTableTested)
  }
  def checkGCC(filteringWithValuesTested: (Array[Set[Int]],Array[Int])=>Array[Set[Int]]):Unit={
    Gcc.checkAC(filteringWithValuesTested)
  }
}