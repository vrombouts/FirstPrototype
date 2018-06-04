package checker

import org.assertj.core.api.{AbstractBooleanAssert, Assertions, ObjectAssert}

/**
  * This class is used can be used for test-suite by considering
  * its functions as assertions. It extends a class from the AssertJ
  * library to ease the writing of unit tests for static filtering
  * algorithms.
  *
  * @param tested : Filter object representing the tested filtering
  *               algorithm
  */
class FilterAssert(tested: Filter)
  extends ObjectAssert[Filter](tested) {

  /**
    * This function asserts that the 'filter' function of
    * 'tested' returns the same filtered domains that the
    * 'filter' function of 'trusted'.
    *
    * @param trusted  : trusted filtering algorithm used to
    *                 test the tested filtering algorithm.
    * @param testArgs : arguments for the tests generated
    * @return itself
    */
  def filterAs(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    isNotNull
    implicit val stats: Statistics = new Statistics("")
    val result: Boolean = CPChecker.check(trusted, tested)
    var errMsg = "Tested and trusted filterings does not filter the same\n"
    //compute error message from statistics
    errMsg += stats.getErrorMsg
    val a: AbstractBooleanAssert[_] = Assertions.assertThat(result).overridingErrorMessage(errMsg)
    a.isTrue
    this
  }

  /**
    * This function asserts that the 'filter' function of
    * 'tested returns filtered domains included in the
    * filtered domains of the 'filter' function of 'trusted'.
    *
    * @param trusted  : trusted filtering algorithm used to
    *                 test the tested filtering algorithm.
    * @param testArgs : arguments for the tests generated
    * @return itself
    */
  def weakerThan(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    isNotNull
    implicit val stats: Statistics = new Statistics("")
    val result: Boolean = CPChecker.stronger(trusted, tested)
    var errMsg = "Tested filtering is not weaker than the trusted\n"
    errMsg += stats.getErrorMsg
    val a: AbstractBooleanAssert[_] = Assertions.assertThat(result).overridingErrorMessage(errMsg)
    a.isTrue
    this
  }

}

/**
  * Companion object easing the reading of the assertions done
  * with the FilterAssert class by allowing lines like this:
  * - assertThat(tested).filterAs(trusted)
  */
object FilterAssert {
  def assertThat(tested: Filter): FilterAssert = new FilterAssert(tested)
}
