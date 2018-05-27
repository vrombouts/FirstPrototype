package checker

import org.assertj.core.api.{AbstractBooleanAssert, Assertions, ObjectAssert}

class FilterAssert(tested: Filter)
  extends ObjectAssert[Filter](tested) {

  def assertThat(tested: Filter) = new FilterAssert(tested)

  /*
  myAssertions.assertThat(myFilter).check(trustedFilter)
   */


  def filterAs(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    implicit val stats: Statistics = new Statistics("")
    val result: Boolean = CPChecker.check(trusted, tested)
    var errMsg = ""
    if (!result) {
      //compute error message from statistics
      errMsg += "Tested and trusted filterings does not filter the same\n"
      errMsg += stats.getErrorMsg
    }
    val a: AbstractBooleanAssert[_] = Assertions.assertThat(result).overridingErrorMessage(errMsg)
    a.isTrue
    this
  }

  /*def strongerThan(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    implicit val stats: Statistics = new Statistics("")
    val result:Boolean = CPChecker.stronger(tested,trusted)
    var errMsg = ""
    if(!result){
      //compute error message from statistics
      errMsg += "Tested filtering must be stronger than the trusted one"
      errMsg += stats.getErrorMsg
    }
    Assertions.assertThat(result).overridingErrorMessage(errMsg).isTrue
    this
  }*/

  def weakerThan(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    implicit val stats: Statistics = new Statistics("")
    val result: Boolean = CPChecker.stronger(trusted, tested)
    var errMsg = ""
    if (!result) {
      //compute error message from statistics
      errMsg += "Tested filtering is not weaker than the trusted\n"
      errMsg += stats.getErrorMsg
    }
    val a: AbstractBooleanAssert[_] = Assertions.assertThat(result).overridingErrorMessage(errMsg)
    a.isTrue
    this
  }

}
