package checker

import checker.Checker.checkAllDifferent
import scala.collection.JavaConverters._


trait JCpC {
    def checkAllDifferentConstraint(): Unit
    def constraint(tab: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

}

abstract class JCpChecker extends JCpC {

  def checkAllDifferentConstraint():Unit = {
    val scalaConstraint = toScala()
    checkAllDifferent(scalaConstraint)
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
  def checkAllDifferentConstraint():Unit = {
    checkAllDifferent(constraint)
  }
  def constraint(vars: Array[Set[Int]]):Array[Set[Int]]

}