package checker

import Conversions._

/**
  * Abstract class representing a filtering algorithm
  * thanks to its 'filter' function
  */
abstract class Filter {
  /**
    * @param variables : array of domains
    * @return filtered domains given the filtering al-
    *         gorithm it implements.
    */
  def filter(variables: Array[Set[Int]]): Array[Set[Int]]
}

/**
  * Abstract class extending the filter class to prop-
  * ose an alternative to the 'filter' function for j-
  * ava developers with the 'filterJava' function.
  */
abstract class JFilter extends Filter {
  override final def filter(variables: Array[Set[Int]]): Array[Set[Int]] = filterJava(variables)

  /**
    *
    * @param variables : array of domains
    * @return filtered domains given the filtering al-
    *         gorithm it implements.
    */
  def filterJava(variables: Array[java.util.Set[Integer]]): Array[java.util.Set[Integer]]

}