package checker

import org.assertj.core.api.ObjectAssert
import org.assertj.core.api.Assertions

class FilterWithStateAssert(tested:FilterWithState)
  extends ObjectAssert[FilterWithState](tested) {

  def assertThat(tested:FilterWithState) = new FilterWithStateAssert(tested)

  /*
  myAssertions.assertThat(myFilter).check(trustedFilter)
   */


  def filterAs(trusted: FilterWithState)(implicit testArgs: TestArgs): FilterWithStateAssert = {
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

  def strongerThan(trusted: FilterWithState)(implicit testArgs: TestArgs): FilterWithStateAssert = {
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

  def weakerThan(trusted: FilterWithState)(implicit testArgs: TestArgs): FilterWithStateAssert = {
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
