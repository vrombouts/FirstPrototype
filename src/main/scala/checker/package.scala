import java.util.function.Function

import scala.collection.JavaConverters._


/**
  * This package object contains the different function to convert
  * objects of the scala language to another language of the JVM
  * and vice versa.
  * Currently, only possesses such functions for java
  */
package object Conversions {

  /**
    * @param sca : array of sets of integers in Scala types
    * @return 'sca' as java types
    */
  implicit def conversionDomainsToJava(sca: Array[Set[Int]]): Array[java.util.Set[Integer]] = {
    val jav: Array[java.util.Set[Integer]] = Array.fill(sca.length)(new java.util.HashSet())
    for (i <- sca.indices) {
      jav(i) = sca(i).map(x => new Integer(x)).asJava
    }
    jav
  }

  /**
    * @param jav : array of sets of integers in java types
    * @return 'sca' as scala types
    */
  implicit def conversionDomainsToScala(jav: Array[java.util.Set[Integer]]): Array[Set[Int]] = {
    val sca: Array[Set[Int]] = Array.fill(jav.length)(Set.empty)
    for (i <- jav.indices) {
      sca(i) = jav(i).asScala.map(x => x.asInstanceOf[Int]).toSet
    }
    sca
  }

  /**
    * @param fun : java Function object representing a checker function
    * @return 'fun' as a scala checker function
    */
  implicit def checkerToScalaFunction(fun: Function[Array[Integer], java.lang.Boolean]): Array[Int] => Boolean = {
    myArray => {
      val ar: Array[Integer] = myArray.map(x => new Integer(x))
      val bool: Boolean = fun.apply(ar)
      bool
    }
  }

  /**
    * @param table : set of arrays of integers in java types
    * @return 'table' as scala types
    */
  implicit def tableToScala(table: java.util.Set[Array[Integer]]): Set[Array[Int]] = {
    var t: Set[Array[Int]] = Set()
    val array = table.toArray(new Array[Array[Integer]](table.size))
    for (tab <- array) {
      val subtab: Array[Int] = Array.fill(tab.length)(0)
      for (i <- tab.indices) subtab(i) = tab(i)
      t = t + subtab
    }
    t
  }
}
