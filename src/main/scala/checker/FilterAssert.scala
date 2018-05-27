package checker

import org.assertj.core.api.ObjectAssert
import org.assertj.core.api.Assertions

class FilterAssert(tested:Filter)
  extends ObjectAssert[Filter](tested) {

  def assertThat(tested:Filter) = new FilterAssert(tested)

  /*
  myAssertions.assertThat(myFilter).check(trustedFilter)
   */


  def filterAs(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    implicit val stats: Statistics = new Statistics("")
    val result:Boolean = CPChecker.check(trusted,tested)
    var errMsg = ""
    if(!result){
      //compute error message from statistics
      //errMsg = stats.getErrorMessage
    }
    Assertions.assertThat(result).overridingErrorMessage(errMsg).isTrue
    this
  }

  def strongerThan(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    implicit val stats: Statistics = new Statistics("")
    val result:Boolean = CPChecker.stronger(tested,trusted)
    var errMsg = ""
    if(!result){
      //compute error message from statistics
      //errMsg = stats.getErrorMessage
    }
    Assertions.assertThat(result).overridingErrorMessage(errMsg).isTrue
    this
  }

  def weakerThan(trusted: Filter)(implicit testArgs: TestArgs): FilterAssert = {
    implicit val stats: Statistics = new Statistics("")
    val result:Boolean = CPChecker.stronger(trusted,tested)
    var errMsg = ""
    if(!result){
      //compute error message from statistics
      //errMsg = stats.getErrorMessage
    }
    Assertions.assertThat(result).overridingErrorMessage(errMsg).isTrue
    this
  }

}
