package checker

import checker.Checker.checkAllDifferent
import scala.collection.JavaConverters._


trait JCpC {
    def checkAllDifferentConstraint(): Unit
    def Constraint(tab: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

}

abstract class JCpChecker extends JCpC {

  def checkAllDifferentConstraint():Unit = {
    val scala_constraint = toScala()
    checkAllDifferent(scala_constraint)
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
    my_array =>{
      val a : Array[java.util.Set[Integer]]= new Array[java.util.Set[Integer]](my_array.length)
      for(i <- my_array.indices){
        val set = my_array(i).asJava
        a(i) = set
      }
      val cons = Constraint(a)
      val result = new Array[Set[Int]](my_array.length)
      for(i <- my_array.indices){
        val set = cons(i).asScala.toSet
        result(i) = set
      }
      result
    }

  }
}

trait ScCpChecker {
  def checkAllDifferentConstraint():Unit = {
    checkAllDifferent(Constraint)
  }
  def Constraint(vars: Array[Set[Int]]):Array[Set[Int]]

}