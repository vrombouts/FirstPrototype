package checker

import Conversions._

abstract class Filter {
  def filter(variables: Array[Set[Int]]): Array[Set[Int]]
}

abstract class JFilter extends Filter {
  override final def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    filterJava(variables)
  }

  def filterJava(variables: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

}