package checker
import Checker.checkConstraint
/*
 * This class is intended to contains functions defining limit cases for some constraints.
 */
object LimitCases {

  def allDifferentLimitCases(): Array[Array[Set[Int]]] ={
    var result:Array[Array[Set[Int]]] = Array()
    result +: Array(Set(0,1,2),Set(0))
    result +: Array(Set(0,1),Set(0,1),Set(0,1,2))
    result +: Array(Set(0),Set(1),Set(2))
    result +: Array(Set(0,1),Set(1,2),Set(2,3),Set(3,4),Set(4,5),Set(2,4))
    result +: Array(Set(0,1,2))
    result +: Array(Set(0,1,2), Set(1))
    result +: Array(Set(0,1,2))
    result +: Array(Set(0,1,2), Set(0), Set(1), Set(2))
    result +: Array(Set(0,1,2,3,4), Set(1), Set(4), Set(3))
    result +: Array(Set(0,1,2), Set(0), Set(2))
    result +: Array(Set(1), Set(1), Set(5,6))
    result +: Array(Set(8,16), Set(16,11), Set(7,8), Set(16), Set(15,8), Set(11,7))
    return result
  }

}
