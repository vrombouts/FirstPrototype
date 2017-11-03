package checker

import java.util

import checker.Checker.{check_AllDifferent}

import scala.collection.JavaConverters._


/**
  * Created by valentin on 02/11/17.
  */
trait JavaCheck {
    def check_allDifferent(): Unit
    def Constraint(tab: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]
    implicit def int2IntegerSet(x: java.util.Set[Int]): java.util.Set[Integer] ={
        var result : java.util.Set[Integer] = new java.util.HashSet[Integer]()
        var iterator = x.iterator()
        while(iterator.hasNext){
            val a: java.lang.Integer = new Integer(iterator.next())
            result.add(a)
        }
        result
    }

    implicit def Integer2intSet(x: Set[Integer]): Set[Int] ={
        var result : Set[Int] = Set[Int]()
        var l = x.toList
        for (elem <- x){
            val a: Int = elem.asInstanceOf[Int]
            result += a
        }
        result
    }

    def scToJ_Set(set:Set[Int]):util.Set[Integer] = {
        val a: util.Set[Integer] = new util.HashSet[Integer]()
        for(i<- set){
            a.add(i)
        }
        a
    }
    def jToSc_Set(set: util.Set[java.lang.Integer]):Set[Int] = {
        var a: Set[Int] = Set[Int]()
        val b = set.asScala.toSet
        for(i <- b) {
            a += i
        }
        a
    }

    def toScala(): Array[Set[Int]] => Array[Set[Int]] ={
        my_array =>{
            var a : Array[java.util.Set[Integer]]= new Array[java.util.Set[Integer]](my_array.length)
            for(i <- my_array.indices){
                var set = my_array(i).asJava
                a(i) = set
            }
            var cons = Constraint(a)
            var result = new Array[Set[Int]](my_array.length)
            for(i <- my_array.indices){
                var set = cons(i).asScala.toSet
                result(i) = set
            }
            result
        }

    }
}

abstract class JC extends JavaCheck {

    def check_allDifferent():Unit = {
        val scala_constraint = toScala
        check_AllDifferent(scala_constraint)
    }
}

trait SC {
  def check_allDifferent():Unit = {
    check_AllDifferent(Constraint)
  }
  def Constraint(vars: Array[Set[Int]]):Array[Set[Int]]

}