package checker

import checker.prunings.BoundDPruning
import org.scalatest.FlatSpec
import checker.FilterAssert._

class FilterAssertTests extends FlatSpec {
  val bdct = new BoundDPruning(Checkers.trueConstraint _)
  val bdcf = new BoundDPruning(Checkers.falseConstraint _)
  val bdca = new BoundDPruning(Checkers.allDifferent())
  implicit val testArgs: TestArgs = new TestArgs
  testArgs.setRangeForAll(-5, 5)

  "FilterAssert" should "not throw an assertion error with filter as against the two same filterings" in {
    assertThat(bdct).filterAs(bdct)
    assertThat(bdcf).filterAs(bdcf)
    assertThat(bdca).filterAs(bdca)
  }

  "FilterAssert" should "not throw an assertion error with weaker than against the two same filterings" in {
    assertThat(bdct).weakerThan(bdct)
    assertThat(bdcf).weakerThan(bdcf)
    assertThat(bdca).weakerThan(bdca)
  }

  "FilterAssert" should "not throw an assertion error with weaker than with true as the weakest" in {
    assertThat(bdct).weakerThan(bdcf)
    assertThat(bdct).weakerThan(bdca)
  }

  "FilterAssert" should "not throw an assertion error with weaker than with -allDifferent is weaker than false-" in {
    assertThat(bdca).weakerThan(bdcf)
  }

  "FilterAssert" should "throw an assertion error if filterAs for true and false checkers" in {
    assertThrows[java.lang.AssertionError](assertThat(bdct).filterAs(bdcf))
  }

  "FilterAssert" should "throw an assertion error if weakerThan for true and false checkers with false the weakest" in {
    assertThrows[java.lang.AssertionError](assertThat(bdcf).weakerThan(bdct))
  }

  "FilterAssert" should "throw an assertion error if weakerThan for true and allDifferent checkers with allDifferent the weakest" in {
    assertThrows[java.lang.AssertionError](assertThat(bdca).weakerThan(bdct))
  }

}
