package checker

import org.scalatest.FlatSpec

class CheckersTests extends FlatSpec {


  /////////testing allDifferent/////////

  "allDiff" should "return true if no two ints are equals" in {
    val allDiff: Array[Int] => Boolean = Checkers.allDifferent()
    assert(allDiff(Array(1)))
    assert(allDiff(Array()))
    assert(allDiff(Array(1, 2)))
    assert(allDiff(Array(0, 2)))
    assert(allDiff(Array(1, 2, 3, 4)))
    assert(allDiff(Array(3, 1, 2, 7, 0, 5)))
    assert(!allDiff(Array(1, 1)))
    assert(!allDiff(Array(1, 2, 3, 5, 1, 6)))
    assert(!allDiff(Array(1, 2, 2, 3, 3)))
    assert(!allDiff(Array(0, 1, 2, 0)))
    assert(!allDiff(Array(254896, 254896)))
  }

  /////////testing element//////////////

  "element" should "consider the array as [x_1,..x_n,i,v] and return x_i=v" in {
    val elem = Checkers.element()
    assert(elem(Array(-1, 2, -86, 256147, 0, -1)))
    assert(elem(Array(-1, 2, -86, 256147, 1, 2)))
    assert(elem(Array(-1, 2, -86, 256147, 2, -86)))
    assert(elem(Array(-1, 2, -86, 256147, 3, 256147)))
    assert(!elem(Array(1, 1, 1)))
    assert(!elem(Array(1, 0, 0)))
    assert(!elem(Array(1, -1, -1)))
    assert(!elem(Array(1, 2, 3, 1, 1)))
    assert(!elem(Array(1, 2, 3, 1, 3)))
  }

  "element(i,v)" should "return an element checker only considering the x as variable and having its i and v fixed" in {
    var elem = Checkers.element(0,0)
    assert(elem(Array(0)))
    assert(elem(Array(0,1)))
    assert(elem(Array(0,52448)))
    assert(elem(Array(0,-987,145,4833,159)))
    assert(!elem(Array(-1)))
    assert(!elem(Array(1)))
    assert(!elem(Array(1561456,0)))
    elem = Checkers.element(1,0)
    assert(elem(Array(1561456, 0)))
    assert(elem(Array(1,0,2)))
    assert(!elem(Array(0)))
  }
  /////////testing sum//////////////////


  "sum(_)" should "be a checker respecting the constraint sum(x)=0" in {
    val sum: Array[Int] => Boolean = Checkers.sum(_)
    assert(sum(Array(0)))
    assert(sum(Array(-1,1)))
    assert(!sum(Array(-1,0)))
    assert(!sum(Array(0,1)))
    assert(sum(Array(1,2,-3)))
    assert(!sum(Array(-1,2,3)))
    assert(sum(Array(-2,1,1)))
    assert(sum(Array(0,0)))
  }

  "sum(0,\"=\")" should "represents a checker respecting the constraint sum(x)=0" in {
    val sum = Checkers.sum(0,"=")
    assert(sum(Array(0)))
    assert(sum(Array(-1,1)))
    assert(!sum(Array(-1,0)))
    assert(!sum(Array(0,1)))
    assert(sum(Array(1,2,-3)))
    assert(!sum(Array(-1,2,3)))
    assert(sum(Array(-2,1,1)))
    assert(sum(Array(0,0)))
  }

  "sum(0,\"<\")" should "represents a checker respecting the constraint sum(x)<0" in {
    val sum = Checkers.sum(0,"<")
    assert(!sum(Array(0)))
    assert(!sum(Array(-1,1)))
    assert(sum(Array(-1,0)))
    assert(!sum(Array(0,1)))
    assert(!sum(Array(1,2,-3)))
    assert(!sum(Array(-1,2,3)))
    assert(!sum(Array(-2,1,1)))
    assert(!sum(Array(0,0)))
  }

  "sum(0,\"<=\")" should "represents a checker respecting the constraint sum(x)<=0" in {
    val sum = Checkers.sum(0,"<=")
    assert(sum(Array(0)))
    assert(sum(Array(-1,1)))
    assert(sum(Array(-1,0)))
    assert(!sum(Array(0,1)))
    assert(sum(Array(1,2,-3)))
    assert(!sum(Array(-1,2,3)))
    assert(sum(Array(-2,1,1)))
    assert(sum(Array(0,0)))
  }

  "sum(0,\">\")" should "represents a checker respecting the constraint sum(x)>0" in {
    val sum = Checkers.sum(0,">")
    assert(!sum(Array(0)))
    assert(!sum(Array(-1,1)))
    assert(!sum(Array(-1,0)))
    assert(sum(Array(0,1)))
    assert(!sum(Array(1,2,-3)))
    assert(sum(Array(-1,2,3)))
    assert(!sum(Array(-2,1,1)))
    assert(!sum(Array(0,0)))
  }

  "sum(0,\">=\")" should "represents a checker respecting the constraint sum(x)>=0" in {
    val sum = Checkers.sum(0,">=")
    assert(sum(Array(0)))
    assert(sum(Array(-1,1)))
    assert(!sum(Array(-1,0)))
    assert(sum(Array(0,1)))
    assert(sum(Array(1,2,-3)))
    assert(sum(Array(-1,2,3)))
    assert(sum(Array(-2,1,1)))
    assert(sum(Array(0,0)))
  }

  "sum(0,\"!=\")" should "represents a checker respecting the constraint sum(x)!=0" in {
    val sum = Checkers.sum(0,"!=")
    assert(!sum(Array(0)))
    assert(!sum(Array(-1,1)))
    assert(sum(Array(-1,0)))
    assert(sum(Array(0,1)))
    assert(!sum(Array(1,2,-3)))
    assert(sum(Array(-1,2,3)))
    assert(!sum(Array(-2,1,1)))
    assert(!sum(Array(0,0)))
  }

  "sum(0,\"??\")" should "not be an existing sum constraint and therefore always return false" in {
    val sum = Checkers.sum(0,"??")
    assert(!sum(Array(0)))
    assert(!sum(Array(-1,1)))
    assert(!sum(Array(-1,0)))
    assert(!sum(Array(0,1)))
    assert(!sum(Array(1,2,-3)))
    assert(!sum(Array(-1,2,3)))
    assert(!sum(Array(-2,1,1)))
    assert(!sum(Array(0,0)))
  }

  /////////testing table////////////////

  val table: Set[Array[Int]] = Set(
    Array(1,2,3),
    Array(1,1,1),
    Array(2,2,2),
    Array(4,4,3),
    Array(1,2,5),
    Array(5,1,2))

  "table(table)" should "perform the table constraint for the solution in table" in {
    val t = Checkers.table(table)
    assert(t(Array(1,1,1)))
    assert(t(Array(1,2,3)))
    assert(t(Array(2,2,2)))
    assert(t(Array(4,4,3)))
    assert(t(Array(1,2,5)))
    assert(t(Array(5,1,2)))
    assert(!t(Array(1,1,2)))
    assert(!t(Array(1,2,4)))
    assert(!t(Array(5,2,2)))
    assert(!t(Array(4,4,4)))
    assert(!t(Array(0,-56,985)))
    assert(!t(Array(2,3,4)))
  }

  "table" should "permit the scala format of a table to create the constraint" in {
    val javaTable: java.util.Set[Array[Integer]] = new java.util.HashSet[Array[Integer]]()
    javaTable.add(Array(new Integer(1), new Integer(1), new Integer(1)))
    javaTable.add(Array(new Integer(1), new Integer(2), new Integer(3)))
    javaTable.add(Array(new Integer(2), new Integer(2), new Integer(2)))
    javaTable.add(Array(new Integer(4), new Integer(4), new Integer(3)))
    javaTable.add(Array(new Integer(1), new Integer(2), new Integer(5)))
    javaTable.add(Array(new Integer(5), new Integer(1), new Integer(2)))
    val t = Checkers.table(javaTable)
    assert(t(Array(1,1,1)))
    assert(t(Array(1,2,3)))
    assert(t(Array(2,2,2)))
    assert(t(Array(4,4,3)))
    assert(t(Array(1,2,5)))
    assert(t(Array(5,1,2)))
    assert(!t(Array(1,1,2)))
    assert(!t(Array(1,2,4)))
    assert(!t(Array(5,2,2)))
    assert(!t(Array(4,4,4)))
    assert(!t(Array(0,-56,985)))
    assert(!t(Array(2,3,4)))
  }

  /////////testing gcc//////////////////

  "gccVar" should "consider the occurrences as variables" in {
    val values = Array(1,2,3)
    val gcc = Checkers.gccVar(values)
    assert(gcc(Array(1,2,3,
      1,1,1)))
    assert(gcc(Array(1,1,1,2,2,2,3,3,3,
      3,3,3)))
    assert(gcc(Array(1,4,1,2,6,2,3,3,3,
      2,2,3)))
    assert(gcc(Array(1,1,2,2,3,3,3,
      2,2,3)))
    assert(gcc(Array(1,2,3,1,2,3,1,2,3,
      3,3,3)))
    assert(!gcc(Array(1,1,2,2,2,3,3,3,
      3,3,3)))
    assert(!gcc(Array(1,1,1,2,2,2,3,3,3,
      2,3,3)))
    assert(!gcc(Array(1,1,1,2,2,2,3,3,3,
      4,3,3)))
    assert(!gcc(Array(1,2,3,1,2,3,1,2,3,
      1,1,1)))
  }

  "gcc" should "consider occurrences as fixed values" in {
    val values = Array(1,2,3)
    val occurrences = Array(2,2,2)
    val gcc = Checkers.gcc(occurrences, values)
    assert(gcc(Array(1,1,2,2,3,3)))
    assert(gcc(Array(1,2,3,1,2,3)))
    assert(gcc(Array(1,2,3,3,2,1)))
    assert(gcc(Array(3,2,1,1,2,3)))
    assert(gcc(Array(1,1,2,2,3,3,4,4)))
    assert(!gcc(Array(1,2,2,3,3)))
    assert(!gcc(Array(1,1,2,3,3)))
    assert(!gcc(Array(1,1,2,2,3)))
    assert(!gcc(Array(1,1,2,2,3,3,3)))
    assert(!gcc(Array(1,1,2,2,2,3,3)))
    assert(!gcc(Array(1,1,1,2,2,3,3)))
    assert(!gcc(Array(1,1,2,2,3,4)))
  }
}
