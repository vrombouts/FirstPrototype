package oscar

import checker.incremental._
import checker.{NoSolutionException, _}
import CPChecker._
import checker.filterings.BoundZFiltering
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints._

/*
 * Incrementally testing the OscaR's bound(Z) consistent filtering for the sum constraint
 * Here, we will test that the algorithm reached bound(Z) consistency and we will also
 * check that the state restoration is correctly performed (push/pop)
 */
object SumBCIncrTest extends App {

  implicit private var solver: CPSolver = new CPSolver
  private var currentVars: Array[CPIntVar] = _

  // setting the test arguments
  testArguments.setSeed(1000)

  // testing the OscaR's filtering algorithm for the constraint sum(x)=i where i evolves between -50 and 50
  for (i <- -50 to 50 by 5) {
    // the trusted algorithm
    val bugFree = new IncrementalFiltering(new BoundZFiltering(Checkers.sum(i, "=")))
    // OscaR's filtering to be tested
    val tested = new FilterWithState {
      override def branchAndFilter(branching: BranchOp): Array[Set[Int]] = sumBranchAndFilter(branching)

      override def setup(variables: Array[Set[Int]]): Array[Set[Int]] = sumSetup(variables, i)
    }

    // checking that the filtered domains returned by both algorithms are the same over some random test instances.
    // i.e. testing that OscaR's filtering reaches bound(Z) consistency for all variables.
    CPChecker.check(bugFree, tested)
  }

  // creating a setup function for the OscaR's filtering reaching fix point
  private def sumSetup(vars: Array[Set[Int]], c: Int): Array[Set[Int]] = {
    solver = CPSolver()
    currentVars = vars.map(x => CPIntVar(x))
    val ad = sum(currentVars).eq(c)
    try {
      solver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
      case _: oscar.cp.core.NoSolutionException => throw new NoSolutionException
    }
    currentVars.map(x => x.toArray.toSet)
  }

  // creating a branch-and-filter function applying the branch operation
  // and then applying the OscaR's filtering
  private def sumBranchAndFilter(branch: BranchOp): Array[Set[Int]] = {
    branch match {
      case _: Push =>
        solver.propagate()
        solver.pushState()
        currentVars.map(x => x.toArray.toSet)
      case _: Pop =>
        solver.pop()
        currentVars.map(x => x.toArray.toSet)
      case r: RestrictDomain =>
        try {
          val constant = r.constant
          val variable = currentVars(r.index)
          var c: oscar.cp.Constraint = null
          r.op match {
            case "=" => c = new EqCons(variable, constant) // x(i)=constant
            case "<" => c = new Le(variable, constant) // x(i)<constant
            case ">" => c = new Gr(variable, constant) // x(i)>constant
            case "!=" => c = new DiffVal(variable, constant) // x(i)!=constant
            case "<=" => c = new LeEq(variable, constant) // x(i)<=constant
            case ">=" => c = new GrEq(variable, constant) // x(i)>=constant
          }
          solver.post(c)
        } catch {
          case _: oscar.cp.core.NoSolutionException =>
            throw new NoSolutionException
          case _: Inconsistency =>
            throw new NoSolutionException
        }
        currentVars.map(x => x.toArray.toSet)
      case _ => currentVars.map(x => x.toArray.toSet)
    }
  }
}
