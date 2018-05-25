package checker

import java.util.function.Function

import org.scalatest.FlatSpec

class HybridFilteringTests extends FlatSpec{

  val bc = new BoundZFiltering(Checkers.allDifferent())
  val ac = new ArcFiltering(Checkers.allDifferent())
  val bcd = new BoundDFiltering(Checkers.allDifferent())
  val rc = new RangeFiltering(Checkers.allDifferent())

  val allDifJavaChecker: Function[Array[Integer], java.lang.Boolean] = {
    domains => {
      var result: Boolean = true
      for (i <- domains.indices) {
        for (j <- 0 until i) {
          if (domains(j).equals(domains(i))) {
            result = false
          }
        }
      }
      result
    }
  }

  "an hybrid filtering" should "filter as ArcFiltering if filterings contains only 1's" in {
    val hf = new HybridFiltering(Array(1,1,1), Checkers.allDifferent())
    val hfJ = new HybridFiltering(Array(new Integer(1),new Integer(1), new Integer(1)),allDifJavaChecker)
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(ac.filter(a)))
    assert(hfJ.filter(a).sameElements(ac.filter(a)))
    a = Array(Set(1),Set(1),Set(2))
    assertThrows[NoSolutionException](hf.filter(a))
    assertThrows[NoSolutionException](hfJ.filter(a))
  }

  "an hybrid filtering" should "filter as boundZFiltering if filterings contains only 2's" in {
    val hf = new HybridFiltering(Array(2,2,2), Checkers.allDifferent())
    val hfJ = new HybridFiltering(Array(new Integer(2),new Integer(2), new Integer(2)),allDifJavaChecker)
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(bc.filter(a)))
    assert(hfJ.filter(a).sameElements(bc.filter(a)))
    a = Array(Set(1),Set(1),Set(2))
    assertThrows[NoSolutionException](hf.filter(a))
    assertThrows[NoSolutionException](hfJ.filter(a))
  }

  "an hybrid filtering" should "filter as boundDFiltering if filterings contains only 3's" in {
    val hf = new HybridFiltering(Array(3,3,3), Checkers.allDifferent())
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(bcd.filter(a)))
    a = Array(Set(1),Set(1),Set(2))
    assertThrows[NoSolutionException](hf.filter(a))
  }

  "an hybrid filtering" should "filter as RangeFiltering if filterings contains only 4's" in {
    val hf = new HybridFiltering(Array(4,4,4), Checkers.allDifferent())
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(rc.filter(a)))
    a = Array(Set(1),Set(1),Set(2))
    assertThrows[NoSolutionException](hf.filter(a))
  }

  "an hybrid filtering" should "filter nothing if filterings contains only 0's" in {
    val hf = new HybridFiltering(Array(0,0,0), Checkers.allDifferent())
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(a))
    a = Array(Set(1),Set(1),Set(2))
    assert(hf.filter(a).sameElements(a))
  }

  "an hybrid filtering" should "perform ac only for the first variable if filterings is [1,0,0]" in {
      val hf = new HybridFiltering(Array(1,0,0), Checkers.allDifferent())
      var a = Array(Set(1,3), Set(1,3), Set(1,4))
      assert(hf.filter(a).sameElements(a))
      a = Array(Set(1),Set(1),Set(2))
      assertThrows[NoSolutionException](hf.filter(a))
      a = Array(Set(2), Set(1,2), Set(3))
      assert(hf.filter(a).sameElements(a))
  }

  "an hybrid filtering" should "perform bcz only for the first variable if filterings is [2,0,0]" in {
    val hf = new HybridFiltering(Array(2,0,0), Checkers.allDifferent())
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(a))
    a = Array(Set(1),Set(1),Set(2))
    assertThrows[NoSolutionException](hf.filter(a))
    a = Array(Set(1,3), Set(1,3), Set(1,3))
    assert(hf.filter(a).sameElements(a))
  }

  "an hybrid filtering" should "perform bcd only for the first variable if filterings is [3,0,0]" in {
    val hf = new HybridFiltering(Array(3,0,0), Checkers.allDifferent())
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(a))
    a = Array(Set(1),Set(1),Set(2))
    assertThrows[NoSolutionException](hf.filter(a))
    a = Array(Set(1,3), Set(1,3), Set(1,3))
    assertThrows[NoSolutionException](hf.filter(a))
  }


  "an hybrid filtering" should "perform range only for the first variable if filterings is [4,0,0]" in {
    val hf = new HybridFiltering(Array(4,0,0), Checkers.allDifferent())
    var a = Array(Set(1,3), Set(1,3), Set(1,4))
    assert(hf.filter(a).sameElements(a))
    a = Array(Set(1),Set(1),Set(2))
    assertThrows[NoSolutionException](hf.filter(a))
    a = Array(Set(1,3), Set(1,3), Set(1,3))
    assert(hf.filter(a).sameElements(a))
  }

}
