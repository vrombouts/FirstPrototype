package oscar

import checker.constraints.incremental._
import checker.NoSolutionException
import checker.constraints.Constraint
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints._

object SumBCIncrTest extends App {

  implicit private var solver: CPSolver = new CPSolver
  private var currentVars: Array[CPIntVar] = _

  private def init(vars: Array[Set[Int]], c: Int): Array[Set[Int]] = {
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

  private def filtering(branch: BranchOp): Array[Set[Int]] = {
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
          var c: Constraint = null
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

  def checker(sol: Array[Int], c: Int): Boolean = {
    if (sol.length == currentVars.length) {
      if (sol.sum == c) return true
      else return false
    }
    true
  }

  Constraint.gen.setSeed(1000)
  for (i <- -50 to 50 by 5)
    Constraint.checkBC(init(_, i), filtering, checker(_, i))
}
