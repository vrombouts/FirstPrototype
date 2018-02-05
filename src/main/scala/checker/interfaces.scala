package checker

import checker.Checker.{checkAllDifferentAC, checkAllDifferentBC}
import scala.collection.JavaConverters._


trait JCpC {
    def checkAllDifferentConstraintAC(): Unit
    def checkAllDifferentConstraintBC(): Unit
    @throws[Exception]
    def constraint(tab: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

}

abstract class JCpChecker extends JCpC {

  def checkAllDifferentConstraintAC():Unit = {
    val scalaConstraint = toScala()
    checkAllDifferentAC(scalaConstraint)
  }
  def checkAllDifferentConstraintBC():Unit = {
    val scalaConstraint = toScala()
    checkAllDifferentBC(scalaConstraint)
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
  def toScala(): Array[Set[Int]] => Array[Set[Int]] ={
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
}

trait ScCpChecker {
  def checkAllDifferentConstraintAC():Unit = {
    checkAllDifferentAC(constraint)
  }
  def checkAllDifferentConstraintBC():Unit = {
    checkAllDifferentBC(constraint)
  }
  def constraint(vars: Array[Set[Int]]):Array[Set[Int]]

}