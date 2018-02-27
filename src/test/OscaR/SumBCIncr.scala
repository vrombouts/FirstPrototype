package OscaR

import checker.{NoSolutionException, _}
import checker.constraints.Constraint
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints._
import oscar.cp.core.CPPropagStrength

object SumBCIncr extends App{

  implicit private var solver:CPSolver = new CPSolver
  private var currentVars:Array[CPIntVar] = _

  private def init(vars: Array[Set[Int]]): Array[Set[Int]] = {
    solver = CPSolver()
    currentVars = vars.map(x => CPIntVar(x))
    val ad = sum(currentVars) > 25
    try {
      solver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
      case _:oscar.cp.core.NoSolutionException => throw new NoSolutionException
    }
    currentVars.map(x => x.toArray.toSet)
  }

  private def filtering(branch:BranchOp):Array[Set[Int]] = {
    branch match{
      case _:Push => {
        solver.pushState()
        currentVars.map(x => x.toArray.toSet)
      }
      case _:Pop => {
        solver.pop()
        currentVars.map(x => x.toArray.toSet)
      }
      case r:RestrictDomain => {
        try {
          r.op match {
            case 0 => solver.add(new EqCons(currentVars(r.index), r.constant))
            case 1 => solver.add(new DiffVal(currentVars(r.index), r.constant))
            case 2 => solver.add(new Le(currentVars(r.index), r.constant))
            case 3 => solver.add(new LeEq(currentVars(r.index), r.constant))
            case 4 => solver.add(new Gr(currentVars(r.index), r.constant))
            case 5 => solver.add(new GrEq(currentVars(r.index), r.constant))
          }
          solver.propagate()
        }catch {
          case _:oscar.cp.core.NoSolutionException => throw new NoSolutionException
          case _:Inconsistency => throw new NoSolutionException
        }
        currentVars.map(x => x.toArray.toSet)
      }
      case _ => currentVars.map(x => x.toArray.toSet)
    }
  }

  def checker(sol:Array[Int]):Boolean= {
    if(sol.length==currentVars.length) {
      var sum = 0
      for (i <- sol)
        sum += i
      if (sum > 25) return true
      else return false
    }
    true
  }

  Constraint.checkAC(init,filtering, checker)
}
