package OscaR

import checker.{NoSolutionException, ScCpChecker}
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.GCCVarAC
import oscar.cp.core.CPPropagStrength

object GCCAC extends App{
  private def gccAC(vars:Array[Set[Int]], values:Array[Int]):Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val assignment = vars.dropRight(values.length).map(x =>CPIntVar(x))
    val cards = vars.drop(vars.length-values.length).map(x =>CPIntVar(x))
    //Attention: Oscar does not take random values. It takes an intervals of values from values(0) to
    //values(0) + cards.length-1
    val ad = new GCCVarAC(assignment,values(0),cards)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    assignment.map(x => x.toArray.toSet)++cards.map(x => x.toArray.toSet)
  }
  ScCpChecker.checkGCC(gccAC)
}
