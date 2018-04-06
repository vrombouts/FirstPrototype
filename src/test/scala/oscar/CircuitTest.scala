package oscar

import checker.NoSolutionException
import checker.constraints.Constraint
import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.core.CPPropagStrength
import oscar.cp.circuit


object CircuitTest extends App{

  private def Circuit(vars: Array[Set[Int]]): Array[Set[Int]] = {
    implicit val testSolver: CPSolver = CPSolver(CPPropagStrength.Strong)
    val variables = vars.map(x => CPIntVar(x))
    val ad = circuit(variables)
    try {
      testSolver.post(ad)
    } catch {
      case _: Inconsistency => throw new NoSolutionException
    }
    variables.map(x => x.toArray.toSet)
  }

  def checker(sol:Array[Int]):Boolean={
    var visited:Array[Boolean]=Array.fill(sol.length)(false)
    def internal(index:Int, acc:Int):Boolean={
      if(sol(index) < 0 || sol(index)>=sol.length)
        return false
      if(visited(sol(index)))
        return false
      visited(sol(index))=true
      if(acc == sol.length){
        if(sol(index)==0)
          return true
        else
          return false
      }
      internal(sol(index), acc+1)
    }
    // change it ! GetNVar ne variarera pas qd scalacheck reduit les tests
    if(sol.length!=Constraint.gen.getNVar())
      return true
    internal(0,1)
  }

  Constraint.gen.setRangeForAll((0,4))
  Constraint.gen.setDensityForAll(0.8)
  Constraint.check(Circuit, checker)
}
