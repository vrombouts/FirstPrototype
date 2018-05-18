package checker

import java.util
import java.util.function.Function

import org.scalatest.FlatSpec
import Conversions._

class ConversionTests extends FlatSpec {

  "calling ConversionDomainsToScala" should "correctly convert [[1,2][2,4]] into Scala type" in {
    val javaSet1: java.util.Set[Integer] = new util.HashSet[Integer]()
    javaSet1.add(new Integer(1))
    javaSet1.add(new Integer(2))
    val javaSet2: java.util.Set[Integer] = new util.HashSet[Integer]()
    javaSet2.add(new Integer(2))
    javaSet2.add(new Integer(4))
    val javaDomain: Array[java.util.Set[Integer]] = Array(javaSet1, javaSet2)
    val result: Array[Set[Int]] = conversionDomainsToScala(javaDomain)
    assert(result.sameElements(Array(Set(1, 2), Set(2, 4))))
  }

  "calling ConversionDomainsToScala" should "correctly convert [[][]] into Scala type" in {
    val javaSet1: java.util.Set[Integer] = new util.HashSet[Integer]()
    val javaSet2: java.util.Set[Integer] = new util.HashSet[Integer]()
    val javaDomain: Array[java.util.Set[Integer]] = Array(javaSet1, javaSet2)
    val result: Array[Set[Int]] = conversionDomainsToScala(javaDomain)
    assert(result.sameElements(Array(Set(), Set())))
  }

  "calling ConversionDomainsToJava" should "correctly convert [[1,2][2,4]] into Java type" in {
    val scalaDomains: Array[Set[Int]] = Array(Set(1, 2), Set(2, 4))
    val result: Array[util.Set[Integer]] = conversionDomainsToJava(scalaDomains)
    for (i <- result.indices) {
      val it: util.Iterator[Integer] = result(i).iterator()
      var scalaSet: Set[Int] = Set()
      while (it.hasNext) {
        scalaSet += it.next()
      }
      assert(scalaSet.equals(scalaDomains(i)))
    }
  }

  "calling ConversionDomainsToJava" should "correctly convert [[][]] into Java type" in {
    val scalaDomains: Array[Set[Int]] = Array(Set(), Set())
    val result: Array[util.Set[Integer]] = conversionDomainsToJava(scalaDomains)
    for (i <- result.indices) {
      val it: util.Iterator[Integer] = result(i).iterator()
      var scalaSet: Set[Int] = Set()
      while (it.hasNext) {
        scalaSet += it.next()
      }
      assert(scalaSet.equals(scalaDomains(i)))
    }
  }

  "calling checkerToScalaFunction" should "correctly convert a java dummy checker" in {
    def checker: Function[Array[Integer], java.lang.Boolean] = {
      domains => true
    }

    val result: (Array[Int]) => Boolean = checkerToScalaFunction(checker)
    val scalaChecker: Array[Int] => Boolean = _ => true
    assert(result(Array(1, 2)), scalaChecker(Array(1, 2)))
  }

  "calling checkerToScalaFunction" should "correctly convert a java allDifferent checker" in {
    def checker: Function[Array[Integer], java.lang.Boolean] = {
      domains => {
        for (i <- domains.indices) {
          for (j <- 0 until i) {
            if (domains(j).equals(domains(i)))
              false
          }
        }
        true
      }
    }

    val result: (Array[Int]) => Boolean = checkerToScalaFunction(checker)
    val scalaChecker: Array[Int] => Boolean = _ => true
    assert(result(Array(1, 2)), scalaChecker(Array(1, 2)))
    assert(result(Array()), scalaChecker(Array()))
    assert(result(Array(1, 2, 3, 4, 2)), scalaChecker(Array(1, 2, 3, 4, 2)))
    assert(result(Array(2, 2)), scalaChecker(Array(2, 2)))
  }

  "calling tableToScalaFunction" should "correctly convert a java table [[1,2,3][4,5,6][5,4,7]] into a scala one " in {
    val javaTable: java.util.Set[Array[Integer]] = new util.HashSet[Array[Integer]]()
    javaTable.add(Array(new Integer(1), new Integer(2), new Integer(3)))
    javaTable.add(Array(new Integer(4), new Integer(5), new Integer(6)))
    javaTable.add(Array(new Integer(5), new Integer(4), new Integer(7)))
    println(javaTable.size())
    val result: Set[Array[Int]] = tableToScala(javaTable)
    println(result.size)
    result.foreach(x => println(x.toList))
    val scalaTable: Set[Array[Int]] = Set(Array(1, 2, 3), Array(4, 5, 6), Array(5, 4, 7))
    scalaTable.foreach(x => println(x.toList))
    assert(result.size == scalaTable.size)
    assert(result.forall(x => scalaTable.exists(y => x.sameElements(y))))
  }

  "calling tableToScalaFunction" should "correctly convert a java table [[1,2,3][4,5][5,4,7]] into a scala one " in {
    val javaTable: java.util.Set[Array[Integer]] = new util.HashSet[Array[Integer]]()
    javaTable.add(Array(new Integer(1), new Integer(2), new Integer(3)))
    javaTable.add(Array(new Integer(4), new Integer(5)))
    javaTable.add(Array(new Integer(5), new Integer(4), new Integer(7)))
    println(javaTable.size())
    val result: Set[Array[Int]] = tableToScala(javaTable)
    println(result.size)
    result.foreach(x => println(x.toList))
    val scalaTable: Set[Array[Int]] = Set(Array(1, 2, 3), Array(4, 5), Array(5, 4, 7))
    scalaTable.foreach(x => println(x.toList))
    assert(result.size == scalaTable.size)
    assert(result.forall(x => scalaTable.exists(y => x.sameElements(y))))
  }

  "calling tableToScalaFunction" should "correctly convert a java table [] into a scala one " in {
    val javaTable: java.util.Set[Array[Integer]] = new util.HashSet[Array[Integer]]()
    println(javaTable.size())
    val result: Set[Array[Int]] = tableToScala(javaTable)
    println(result.size)
    result.foreach(x => println(x.toList))
    val scalaTable: Set[Array[Int]] = Set()
    scalaTable.foreach(x => println(x.toList))
    assert(result.size == scalaTable.size)
    assert(result.forall(x => scalaTable.exists(y => x.sameElements(y))))
  }

}
