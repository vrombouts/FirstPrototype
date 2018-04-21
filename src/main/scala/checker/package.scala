import java.util.function.{BiFunction, Function}

import checker.constraints.incremental.BranchOp

import scala.collection.JavaConverters._


package object Conversions {
  implicit def conversionDomainsToScala(sca: Array[Set[Int]]): Array[java.util.Set[Integer]] = {
    val jav: Array[java.util.Set[Integer]] = Array.fill(sca.length)(new java.util.HashSet())
    for (i <- sca.indices) {
      jav(i) = sca(i).map(x => new Integer(x)).asJava
    }
    jav
  }

  implicit def conversionDomainsToJava(jav: Array[java.util.Set[Integer]]): Array[Set[Int]] = {
    val sca: Array[Set[Int]] = Array.fill(jav.length)(Set.empty)
    for (i <- jav.indices) {
      sca(i) = jav(i).asScala.map(x => x.asInstanceOf[Int]).toSet
    }
    sca
  }

  implicit def branchToScalaFunction(fun: Function[BranchOp, Array[java.util.Set[Integer]]]): BranchOp => Array[Set[Int]] = {
    branchOp: BranchOp => {
      fun.apply(branchOp)
    }
  }

  implicit def tableFilterToScalaFunction(fun: BiFunction[Array[java.util.Set[Integer]], java.util.Set[Array[Integer]], Array[java.util.Set[Integer]]])
  : (Array[Set[Int]], Set[Array[Int]]) => Array[Set[Int]] = {
    (myArray, myTable) => {
      val javaTable: java.util.Set[Array[Integer]] = new java.util.HashSet[Array[Integer]]()
      for (tableElement <- myTable) {
        val javaTableElement: Array[Integer] = new Array[Integer](tableElement.length)
        for (i <- tableElement.indices) {
          javaTableElement(i) = tableElement(i)
        }
        javaTable.add(javaTableElement)
      }
      fun.apply(myArray, javaTable)
    }
  }

  implicit def filterToScalaFunction(fun: Function[Array[java.util.Set[Integer]], Array[java.util.Set[Integer]]]): Array[Set[Int]] => Array[Set[Int]] = {
    myArray => {
      fun.apply(myArray)
    }
  }

  implicit def checkerToScalaFunction(fun: Function[Array[Integer], java.lang.Boolean]): Array[Int] => Boolean = {
    myArray => {
      val ar: Array[Integer] = myArray.map(x => new Integer(x))
      val bool: Boolean = fun.apply(ar)
      bool
    }
  }

  implicit def tableToScala(table: java.util.Set[Array[Integer]]): Set[Array[Int]] = {
    var t: Set[Array[Int]] = Set()
    val array = table.toArray(new Array[Array[Integer]](table.size))
    for(tab <- array){
      val subtab: Array[Int] = Array.fill(tab.length)(0)
      for(i <- tab.indices) subtab(i) = tab(i)
      t = t + subtab
    }
    t
  }
}
