package checker

import checker.Checker.check_AllDifferent
import scala.collection.JavaConverters._


/**
  * Created by valentin on 02/11/17.
  */
trait JCpC {
    def check_allDifferent(): Unit
    def Constraint(tab: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

}

abstract class JCpChecker extends JCpC {

  def check_allDifferent():Unit = {
        val scala_constraint = toscala()
        check_AllDifferent(scala_constraint)
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
  def toscala(): Array[Set[Int]] => Array[Set[Int]] ={
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
  def check_allDifferent():Unit = {
    check_AllDifferent(Constraint)
  }
  def Constraint(vars: Array[Set[Int]]):Array[Set[Int]]

}