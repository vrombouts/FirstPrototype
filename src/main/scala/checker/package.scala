import java.util.function.{BiFunction, Function}

import checker.BranchOp

import scala.collection.JavaConverters._


package object Conversions {
  implicit def conversionDomainsToScala(sca: Array[Set[Int]]):Array[java.util.Set[Integer]] ={
    val jav:Array[java.util.Set[Integer]] = Array.fill(sca.length)(new java.util.HashSet())
    for(i <- sca.indices){
      jav(i) = sca(i).map(x=> new Integer(x)).asJava
    }
    jav
  }
  implicit def conversionDomainsToJava(jav:Array[java.util.Set[Integer]]):Array[Set[Int]] = {
    val sca:Array[Set[Int]] = Array.fill(jav.length)(Set.empty)
    for(i <- jav.indices){
      sca(i) = jav(i).asScala.map(x => x.asInstanceOf[Int]).toSet
    }
    sca
  }

  implicit def branchToScalaFunction(fun: Function[BranchOp,Array[java.util.Set[Integer]]]): BranchOp => Array[Set[Int]] = {
    branchOp: BranchOp => {
      val cons = fun.apply(branchOp)
      var result:Array[Set[Int]] = new Array[Set[Int]](cons.length)
      for(i <- cons.indices){
        val set = cons(i).asScala.toSet
        result(i) =  set
      }
      result
    }
  }
  implicit def tableFilterToScalaFunction(fun: BiFunction[Array[java.util.Set[Integer]],java.util.Set[Array[Integer]],Array[java.util.Set[Integer]]])
  : (Array[Set[Int]],Set[Array[Int]]) => Array[Set[Int]] = {
    (myArray,myTable) =>{
      val javaArray : Array[java.util.Set[Integer]]= new Array[java.util.Set[Integer]](myArray.length)
      for(i <- myArray.indices){
        val set = myArray(i).asJava
        javaArray(i) = set
      }
      val javaTable : java.util.Set[Array[Integer]] = new java.util.HashSet[Array[Integer]]()
      for(tableElement<- myTable){
        val javaTableElement:Array[Integer] = new Array[Integer](tableElement.length)
        for(i<- tableElement.indices){
          javaTableElement(i)=tableElement(i)
        }
        javaTable.add(javaTableElement)
      }
      val cons = fun.apply(javaArray,javaTable)
      val result = new Array[Set[Int]](myArray.length)
      for(i <- myArray.indices){
        val set = cons(i).asScala.toSet
        result(i) = set
      }
      result
    }
  }
  implicit def filterToScalaFunction(fun: Function[Array[java.util.Set[Integer]],Array[java.util.Set[Integer]]]): Array[Set[Int]] => Array[Set[Int]] = {
    myArray =>{
      val a : Array[java.util.Set[Integer]]= new Array[java.util.Set[Integer]](myArray.length)
      for(i <- myArray.indices){
        val set = myArray(i).asJava
        a(i) = set
      }
      val cons = fun.apply(a)
      val result = new Array[Set[Int]](myArray.length)
      for(i <- myArray.indices){
        val set = cons(i).asScala.toSet
        result(i) = set
      }
      result
    }
  }
  implicit def checkerToScalaFunction(fun: Function[Array[Integer],java.lang.Boolean]): Array[Int] => Boolean = {
    myArray =>{
      val ar: Array[Integer] = myArray.map(x => new Integer(x))
      val bool: Boolean = fun.apply(ar)
      bool
    }
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
}
