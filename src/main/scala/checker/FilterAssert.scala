package checker

import org.assertj.core.api.{AbstractBooleanAssert, Assertions, ObjectAssert}

class FilterAssert(tested: Filter)
  extends ObjectAssert[Filter](tested) {

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

object FilterAssert {
  def assertThat(tested: Filter): FilterAssert = new FilterAssert(tested)
}
