package checker
import Checker.checkConstraint
/*
 * This class is intended to contains functions defining limit cases for some constraints.
 */
object LimitCases {

  def allDifferentLimitCases(): Array[Array[Set[Int]]] ={
    Array(
      Array(Set(0,1,2),Set(0)),
      Array(Set(0,1),Set(0,1),Set(0,1,2)),
      Array(Set(0),Set(1),Set(2)),
      Array(Set(0,1),Set(1,2),Set(2,3),Set(3,4),Set(4,5),Set(2,4)),
      Array(Set(0,1,2)),
      Array(Set(0,1,2), Set(1)),
      Array(Set(0,1,2)),
      Array(Set(0,1,2), Set(0), Set(1), Set(2)),
      Array(Set(0,1,2,3,4), Set(1), Set(4), Set(3)),
      Array(Set(0,1,2), Set(0), Set(2)),
      Array(Set(1), Set(1), Set(5,6)),
      Array(Set(8,16), Set(16,11), Set(7,8), Set(16), Set(15,8), Set(11,7))
    )
  }
  def sumLimitCases: Array[Array[Set[Int]]] = {
    Array(
      Array(Set(Integer.MAX_VALUE), Set(1), Set(Integer.MIN_VALUE)),
      Array(Set(Integer.MIN_VALUE), Set(-1),Set(Integer.MAX_VALUE)),
      Array(Set(2), Set(48),Set(50)),
      Array(Set(2,50-2),Set(2,50-2), Set(50)),
      Array(Set(1,2), Set(2,4),Set(50-6), Set(50)),// 50 == sMax
      Array(Set(1,2), Set(2,4),Set(50-5), Set(50)),// 50 > sMax
      Array(Set(1,2), Set(2,4),Set(50-7), Set(50)),// 50 < sMax
      Array(Set(1,2), Set(2,4),Set(50-3), Set(50)),// 50 == sMin
      Array(Set(1,2), Set(2,4),Set(50-2), Set(50)),// 50 > sMin
      Array(Set(1,2), Set(2,4),Set(50-4), Set(50)),// 50 < sMin
      Array(Set(2), Set(2),Set(50-4), Set(50))     // 50 == sMin == sMax
    )
  }

}
