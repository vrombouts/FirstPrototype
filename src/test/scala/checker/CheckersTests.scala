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
  /////////testing table////////////////
  /////////testing gcc//////////////////
}
