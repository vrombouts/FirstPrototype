package checker.filterings

import java.util.function.Function

import Conversions.checkerToScalaFunction
import checker._

/**
  * This class implements the Filter abstract class to represent
  * an arc consistent filtering algorithm. The constraint of the
  * filtering algorithm is defined by the 'checker' function gi-
  * ven to its constructor.
  *
  * @param filterings : an array precising the consistency the va-
  *                   riable of the same index should reach.
  *                   - 1 for the arc consistency
  *                   - 2 for the bound(Z) consistency
  *                   - 3 for the bound(D) consistency
  *                   - 4 for the range consistency
  *                   - ? no filtering
  * @param checker    : a boolean function taking an instantiation
  *                   as argument. It should return true if the g-
  *                   iven instantiation respects the constraint
  *                   it defines.
  */
class HybridFiltering(filterings: Array[Int], checker: Array[Int] => Boolean) extends Filter {
  //java constructor
  def this(filterings: Array[Integer], checker: Function[Array[Integer], java.lang.Boolean]) = this(filterings.map(x => x.asInstanceOf[Int]), checkerToScalaFunction(checker))


  val arc = new ArcFiltering(checker)
  val boundZ = new BoundZFiltering(checker)
  val boundD = new BoundDFiltering(checker)
  val range = new RangeFiltering(checker)

  /**
    * @param variables : array of domains
    * @return the filtered domains respecting their asked consistency
    *         from 'filterings' according to the 'checker' function
    */
  override def filter(variables: Array[Set[Int]]): Array[Set[Int]] = {
    val vars = variables
    //The ac variables can directly be obtained thanks to ArcFiltering.
    if (filterings.contains(1)) {
      val acVars = arc.filter(variables)
      for (i <- vars.indices)
        if (filterings(i) == 1) vars(i) = acVars(i)
    }
    // the bound and range consistent variables must recursively be reduced.
    val intervals = vars.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
    filterIntervals(vars, intervals)
    intervals.map(x => x.dom)
  }


  private[this] def filterIntervals(vars: Array[Set[Int]], intervals: Array[Interval]): Unit = {
    if (intervals.indices.foldLeft(false) { (acc, i) =>
      if ((filterings(i) == 2 && boundZ.changeBounds(i, intervals)) ||
        (filterings(i) == 3 && boundDChangeBounds(i, intervals)) ||
        (filterings(i) == 4 && range.filterInterval(i, intervals)))
        true
      else acc
    })
      filterIntervals(vars, intervals)
  }

  private[this] def boundDChangeBounds(i: Int, intervals: Array[Interval]): Boolean = {
    val vars = intervals.map(x => x.dom)
    val result = boundD.changeBounds(i, vars)
    intervals(i).dom = vars(i)
    result
  }


}
