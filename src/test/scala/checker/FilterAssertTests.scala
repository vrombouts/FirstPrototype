package checker

import checker.prunings.BoundDPruning
import org.scalatest.FlatSpec

class FilterAssertTests extends FlatSpec{
  val bdct = new BoundDPruning(Checkers.trueConstraint _)
  val bdcf = new BoundDPruning(Checkers.falseConstraint _)
  val bdca = new BoundDPruning(Checkers.allDifferent())
  val myAssert = new FilterAssert(null)
  implicit val testArgs: TestArgs = new TestArgs
  testArgs.setRangeForAll(-5,5)

  "FilterAssert" should "not throw an assertion error with filter as against the two same filterings" in {
    myAssert.assertThat(bdct).filterAs(bdct)
    myAssert.assertThat(bdcf).filterAs(bdcf)
    myAssert.assertThat(bdca).filterAs(bdca)
  }

  "FilterAssert" should "not throw an assertion error with weaker than against the two same filterings" in {
    myAssert.assertThat(bdct).weakerThan(bdct)
    myAssert.assertThat(bdcf).weakerThan(bdcf)
    myAssert.assertThat(bdca).weakerThan(bdca)
  }

  "FilterAssert" should "not throw an assertion error with weaker than with true as the weakest" in {
    myAssert.assertThat(bdct).weakerThan(bdcf)
    myAssert.assertThat(bdct).weakerThan(bdca)
  }

  "FilterAssert" should "not throw an assertion error with weaker than with -allDifferent is weaker than false-" in {
    myAssert.assertThat(bdca).weakerThan(bdcf)
  }

  "FilterAssert" should "throw an assertion error if filterAs for true and false checkers" in {
    assertThrows[java.lang.AssertionError](myAssert.assertThat(bdct).filterAs(bdcf))
  }

  "FilterAssert" should "throw an assertion error if weakerThan for true and false checkers with false the weakest" in {
    assertThrows[java.lang.AssertionError](myAssert.assertThat(bdcf).weakerThan(bdct))
  }

  "FilterAssert" should "throw an assertion error if weakerThan for true and allDifferent checkers with allDifferent the weakest" in {
    assertThrows[java.lang.AssertionError](myAssert.assertThat(bdca).weakerThan(bdct))
  }

}
