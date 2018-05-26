package checker

import java.util.function.Function
import scala.collection.JavaConverters._

import Conversions.checkerToScalaFunction

/**
  * hybrid filtering but which can apply every type of consistencies implemented by CPChecker.
  *
  * @param filterings : array corresponding to each variable telling the consistency apply to it.
  *                   1 for Arc consistency,
  *                   2 for Bound(Z) consistency,
  *                   3 for Bound(D) consistency,
  *                   4 for Range consistency.
  * @param checker    : the checker function representing the constraint.
  */

class HybridPruning(filterings: Array[Int], checker: Array[Int] => Boolean) extends Filter {

  def this(filterings: Array[Integer], checker: Function[Array[Integer], java.lang.Boolean]) = this(filterings.map(x => x.asInstanceOf[Int]), checkerToScalaFunction(checker))


  val arc = new ArcPruning(checker)
  val boundZ = new BoundZPruning(checker)
  val boundD = new BoundDPruning(checker)
  val range = new RangePruning(checker)

  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val vars = variables
    //The ac variables can directly be obtained thanks to ACFiltering.
    if(filterings.contains(1)) {
      val acVars = arc.filter(variables)
      for (i <- vars.indices) {
        if (filterings(i) == 1) vars(i) = acVars(i)
      }
    }
    // the BC and RC variable must recursively be reduced.
    val intervals = vars.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
    filterIntervals(vars, intervals)
    intervals.map(x => x.dom)
  }


  private[this] def filterIntervals(vars: Array[Set[Int]], intervals: Array[Interval]): Unit = {
    if (intervals.indices.foldLeft(false) { (acc, i) =>
      if (filterings(i) == 2) if (boundZ.changeBounds(i, intervals)) true else acc
      else if (filterings(i) == 3) if (boundDChangeBounds(i, intervals)) true else acc
      else if (filterings(i) == 4) if (range.filterInterval(i, intervals)) true else acc
      else acc
    })
      filterIntervals(vars, intervals)
  }

  def boundDChangeBounds(i: Int, intervals: Array[Interval]): Boolean = {
    val vars = intervals.map(x => x.dom)
    val result = boundD.changeBounds(i, vars)
    intervals(i).dom = vars(i)
    result
  }


}
