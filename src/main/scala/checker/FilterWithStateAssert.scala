package checker

import org.assertj.core.api.{AbstractBooleanAssert, Assertions, ObjectAssert}

/**
  * This class is used can be used for test-suite by considering
  * its functions as assertions. It extends a class from the AssertJ
  * library to ease the writing of unit tests for incremental filtering
  * algorithms.
  *
  * @param tested : FilterWithState object representing the tested
  *               filtering algorithm
  */
class FilterWithStateAssert(tested: FilterWithState)
  extends ObjectAssert[FilterWithState](tested) {

  /**
    * This function asserts that the 'setup' and 'branchAndFilter'
    * functions of 'tested' returns the same filtered domains that
    * the 'setup' and 'branchAndFilter' functions of 'trusted'.
    *
    * @param trusted  : trusted filtering algorithm used to
    *                 test the tested filtering algorithm.
    * @param testArgs : arguments for the tests generated
    * @return itself
    */
  def filterAs(trusted: FilterWithState)(implicit testArgs: TestArgs): FilterWithStateAssert = {
    isNotNull
    implicit val stats: Statistics = new Statistics("")
    val result: Boolean = CPChecker.check(trusted, tested)
    var errMsg = "Tested and trusted filtering do not filter the same\n"
    //compute error message from statistics
    errMsg += stats.getErrorMsg
    val a: AbstractBooleanAssert[_] = Assertions.assertThat(result).overridingErrorMessage(errMsg)
    a.isTrue
    this
  }

  /**
    * This function asserts that the 'setup' and 'branchAndFilter'
    * functions of 'tested' returns filtered domains included in
    * the 'setup' and 'branchAndFilter' functions of 'trusted'.
    *
    * @param trusted  : trusted filtering algorithm used to
    *                 test the tested filtering algorithm.
    * @param testArgs : arguments for the tests generated
    * @return itself
    */
  def weakerThan(trusted: FilterWithState)(implicit testArgs: TestArgs): FilterWithStateAssert = {
    isNotNull
    implicit val stats: Statistics = new Statistics("")
    val result: Boolean = CPChecker.stronger(trusted, tested)
    var errMsg = "tested filtering is not weaker than the trusted one\n"
    //compute error message from statistics
    errMsg += stats.getErrorMsg
    val a: AbstractBooleanAssert[_] = Assertions.assertThat(result).overridingErrorMessage(errMsg)
    a.isTrue
    this
  }

}

/**
  * Companion object easing the reading of the assertions done
  * with the FilterWithStateAssert class by allowing lines like this:
  * - assertThat(tested).filterAs(trusted)
  */
object FilterWithStateAssert {
  def assertThat(tested: FilterWithState) = new FilterWithStateAssert(tested)
}
