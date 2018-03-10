package OscaR

import checker.{NoSolutionException, _}
import checker.constraints.Constraint
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints._

object SumBCIncr extends App{

  implicit private var solver:CPSolver = new CPSolver
  private var currentVars:Array[CPIntVar] = _

  private def init(vars: Array[Set[Int]], c:Int): Array[Set[Int]] = {
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

  private def filtering(branch:BranchOp):Array[Set[Int]] = {
    branch match{
      case _:Push =>
        solver.pushState()
        solver.propagate()
        currentVars.map(x => x.toArray.toSet)
      case _:Pop =>
        solver.pop()
        solver.propagate()
        currentVars.map(x => x.toArray.toSet)
      case r:RestrictDomain =>
        try {
          r.op match {
            case "=" => solver.post(new EqCons(currentVars(r.index), r.constant))
            case "!=" => solver.post(new DiffVal(currentVars(r.index), r.constant))
            case "<" => solver.post(new Le(currentVars(r.index), r.constant))
            case "<=" => solver.post(new LeEq(currentVars(r.index), r.constant))
            case ">" => solver.post(new Gr(currentVars(r.index), r.constant))
            case ">=" => solver.post(new GrEq(currentVars(r.index), r.constant))
          }
        }catch {
          case _:oscar.cp.core.NoSolutionException => throw new NoSolutionException
          case _:Inconsistency => throw new NoSolutionException
        }
        currentVars.map(x => x.toArray.toSet)
      case _ => currentVars.map(x => x.toArray.toSet)
    }
  }

  def checker(sol:Array[Int],c:Int):Boolean= {
    if(sol.length==currentVars.length) {
      var sum = 0
      for (i <- sol)
        sum += i
      if (sum == c) return true
      else return false
    }
    true
  }

  for(i <- 1 to 100 by 5)
    Constraint.checkBC(init(_,i),filtering, checker(_,i))
}
