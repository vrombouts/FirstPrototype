package unit.Statistics

import checker.StrictStatistics
import unit.UnitSpec

class StrictStatisticsTests extends UnitSpec{

  val stats = new StrictStatistics

  "printNumber with a number with less than 10 digits ('-' included)" should " return a string of size 11 containing that number" in {
    var s:String = stats.printNumber(0)
    assert(s.length==11 && s.contains("0"))
    s = stats.printNumber(10)
    assert(s.length==11 && s.contains("10"))
    s = stats.printNumber(100)
    assert(s.length==11 && s.contains("100"))
    s = stats.printNumber(1000)
    assert(s.length==11 && s.contains("1000"))
    s = stats.printNumber(10000)
    assert(s.length==11 && s.contains("10000"))
    s = stats.printNumber(100000)
    assert(s.length==11 && s.contains("100000"))
    s = stats.printNumber(1000000)
    assert(s.length==11 && s.contains("1000000"))
    s = stats.printNumber(10000000)
    assert(s.length==11 && s.contains("10000000"))
    s = stats.printNumber(100000000)
    assert(s.length==11 && s.contains("100000000"))
    s = stats.printNumber(1000000000)
    assert(s.length==11 && s.contains("1000000000"))
    s = stats.printNumber(-1)
    assert(s.length==11 && s.contains("-1"))
    s = stats.printNumber(-10)
    assert(s.length==11 && s.contains("-10"))
    s = stats.printNumber(-100)
    assert(s.length==11 && s.contains("-100"))
    s = stats.printNumber(-1000)
    assert(s.length==11 && s.contains("-1000"))
    s = stats.printNumber(-10000)
    assert(s.length==11 && s.contains("-10000"))
    s = stats.printNumber(-100000)
    assert(s.length==11 && s.contains("-100000"))
    s = stats.printNumber(-1000000)
    assert(s.length==11 && s.contains("-1000000"))
    s = stats.printNumber(-10000000)
    assert(s.length==11 && s.contains("-10000000"))
    s = stats.printNumber(-100000000)
    assert(s.length==11 && s.contains("-100000000"))
  }

  "strictStatistics incorrectDomains" should "return false only if the two array possess the same sets in the same order" in {
    var a = Array(Set(1),Set(2,1))
    var b = Array(Set(1),Set(1,2))
    assert(!stats.incorrectDomains(a,b))
    b = Array(Set(2,1), Set(1))
    assert(stats.incorrectDomains(a,b))
    a = Array(Set(1))
    assert(stats.incorrectDomains(a,b))
    b = Array(Set(1))
    assert(!stats.incorrectDomains(a,b))
    a = Array()
    assert(stats.incorrectDomains(a,b))
    b=Array()
    assert(!stats.incorrectDomains(a,b))
  }

  "strictStatistics strictDomainComparison" should "increment nbRemovingValues if ourReduceDomains is smaller than (and included in) init" in {
    val a = Array(Set(1),Set(1,2,3), Set(1,2))
    val b = Array(Set(1),Set(1,2), Set(1,2))
    val remove = stats.getNbRemovingValueTests
    stats.strictDomainComparison(b,b,a,result = true)
    assert(remove+1==stats.getNbRemovingValueTests)
    stats.strictDomainComparison(b,b,b,result = true)
    assert(remove+1==stats.getNbRemovingValueTests)
  }

  "strictStatistics strictDomainComparison" should "increment nbRemoveNoValues if ourReduceDomains has the same elements as init" in {
    val a = Array(Set(1),Set(1,2,3), Set(1,2))
    val b = Array(Set(1),Set(1,2), Set(1,2))
    val remove = stats.getNbRemoveNoValueTests
    stats.strictDomainComparison(b,b,a,result = true)
    assert(remove==stats.getNbRemoveNoValueTests)
    stats.strictDomainComparison(b,b,b,result = true)
    assert(remove+1==stats.getNbRemoveNoValueTests)
  }

  "strictStatistics strictDomainComparison" should "increment nbFailRemovingValues if nbRemovingValues is incremented and result is false" in {
    val a = Array(Set(1),Set(1,2,3), Set(1,2))
    val b = Array(Set(1),Set(1,2), Set(1,2))
    val remove = stats.getNbRemovingValueTests
    val remove2 = stats.getNbFailedRemovingValueTests
    stats.strictDomainComparison(b,b,a,result = true)
    assert(remove+1==stats.getNbRemovingValueTests &&
      remove2 == stats.getNbFailedRemovingValueTests)
    stats.strictDomainComparison(b,b,a,result = false)
    assert(remove+2==stats.getNbRemovingValueTests &&
      remove2+1 == stats.getNbFailedRemovingValueTests)
    stats.strictDomainComparison(b,b,b,result = true)
    assert(remove+2==stats.getNbRemovingValueTests &&
      remove2+1 == stats.getNbFailedRemovingValueTests)
    stats.strictDomainComparison(b,b,b,result = false)
    assert(remove+2==stats.getNbRemovingValueTests &&
      remove2+1 == stats.getNbFailedRemovingValueTests)
  }

  "strictStatistics strictDomainComparison" should "increment nbFailRemoveNoValues if nbRemoveNoValues is incremented and result is false" in {
    val a = Array(Set(1),Set(1,2,3), Set(1,2))
    val b = Array(Set(1),Set(1,2), Set(1,2))
    val remove = stats.getNbRemoveNoValueTests
    val remove2 = stats.getNbFailedRemoveNoValueTests
    stats.strictDomainComparison(b,b,a,result = true)
    assert(remove==stats.getNbRemoveNoValueTests &&
      remove2 == stats.getNbFailedRemoveNoValueTests)
    stats.strictDomainComparison(b,b,a,result = false)
    assert(remove==stats.getNbRemoveNoValueTests &&
      remove2 == stats.getNbFailedRemoveNoValueTests)
    stats.strictDomainComparison(b,b,b,result = true)
    assert(remove+1==stats.getNbRemoveNoValueTests &&
      remove2 == stats.getNbFailedRemoveNoValueTests)
    stats.strictDomainComparison(b,b,b,result = false)
    assert(remove+2==stats.getNbRemoveNoValueTests &&
      remove2+1 == stats.getNbFailedRemoveNoValueTests)
  }

  "strictStatistics comparison" should "return true only if returnValues(1) is the same as returnValues(2)" in {
    var v = Array(
      Array(Set(1)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(stats.comparison(v))
    v = Array(
      Array(Set(1),Set(2)),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(stats.comparison(v))
    v = Array(
      Array(Set(1)),
      Array(Set(1),Set(2)),
      Array(Set(1),Set(2))
    )
    assert(stats.comparison(v))
    v = Array(
      Array(),
      Array(Set(1)),
      Array(Set(1))
    )
    assert(stats.comparison(v))
    v = Array(
      Array(),
      Array(),
      Array()
    )
    assert(stats.comparison(v))
  }

}
