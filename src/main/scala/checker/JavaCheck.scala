package checker

import checker.Checker.{check_AllDifferent, toScala2}


/**
  * Created by valentin on 02/11/17.
  */
trait JavaCheck {
    def check_allDifferent(): Unit
    def Constraint(tab: java.util.ArrayList[java.util.Set[java.lang.Integer]]): java.util.ArrayList[java.util.Set[java.lang.Integer]]
}

abstract class JC extends JavaCheck {
  def check_allDifferent():Unit = {
    val scala_constraint = toScala2(this)
    check_AllDifferent(scala_constraint)
  }
}

trait SC {
  def check_allDifferent():Unit = {
    check_AllDifferent(Constraint)
  }
  def Constraint(vars: Array[Set[Int]]):Array[Set[Int]]

}