package checker.constraints

import checker.constraints.incremental.BranchOp
import checker._
import org.scalacheck.Prop.forAll

import scala.collection.mutable


object Constraint extends Checker {

  private var isAC: Boolean = true
  private var checkFunction: Array[Int] => Boolean = _

  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if (isAC) applyAC(variables, checkFunction)
    else applyBC(variables, checkFunction)
  }

  def checkAC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    isAC = true
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringTested)
    }.check
    //TODO: add simple case limit possible for all constraints
    println(Statistics.statisticsToString())
  }

  def checkBC(filteringTested: Array[Set[Int]] => Array[Set[Int]], checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    isAC = false
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, filteringTested)
    }.check
    //TODO: add simple case limit possible for all constraints
  }

  def checkAC(init: Array[Set[Int]] => Array[Set[Int]],
              filtering: BranchOp => Array[Set[Int]],
              checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    isAC = true
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, init, filtering)
    }.check
  }

  def checkBC(init: Array[Set[Int]] => Array[Set[Int]],
              filtering: BranchOp => Array[Set[Int]],
              checker: Array[Int] => Boolean): Unit = {
    checkFunction = checker
    isAC = false
    forAll(gen.gen) { x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray, init, filtering)
    }.check
  }


  // Applying AC with pruning //
  private def firstStream(variable: Set[Int]): Stream[List[Int]] = {
    variable.map(x => List(x)).toStream
  }

  private def perOneValueStreamConstructor(solution: List[Int], variable: Set[Int], str: Stream[List[Int]]): Stream[List[Int]] = {
    if (variable.isEmpty) return str
    val current = variable.last
    (current :: solution) #:: perOneValueStreamConstructor(solution, variable - current, str)
  }

  private def nthStream(variable: Set[Int], previousStream: Stream[List[Int]]): Stream[List[Int]] = {
    if (previousStream.isEmpty) return Stream.empty[List[Int]]
    val solution = previousStream.head
    val str = (variable.last :: solution) #:: nthStream(variable, previousStream.tail)
    val vars = variable - variable.last
    perOneValueStreamConstructor(solution, vars, str)

  }

  @throws[NoSolutionException]
  private def cartesianProduct(variables: Array[Set[Int]], constraint: Array[Int] => Boolean): Stream[List[Int]] = {
    if (variables.length < 1) throw new NoSolutionException
    var str = firstStream(variables.last).filter(x => constraint(x.toArray))
    if (variables.length == 1) return str
    for (i <- variables.length - 2 to 0 by -1) {
      str = nthStream(variables(i), str).filter(x => constraint(x.toArray))
    }
    str
  }

  private def toDomainsAC(solutions: Stream[List[Int]]): Array[Set[Int]] = {
    val variables: Array[Set[Int]] = Array.fill(solutions.head.size)(Set.empty)
    solutions.foreach { sol =>
      for (i <- variables.indices) {
        variables(i) += sol(i)
      }
    }
    variables
  }

  def toDomainsAC(solutions: Array[Array[Int]]): Array[Set[Int]] = {
    val variables: Array[Set[Int]] = Array.fill(solutions(0).length)(Set.empty)
    solutions.foreach { sol =>
      for (i <- variables.indices) {
        variables(i) += sol(i)
      }
    }
    variables
  }

  @throws[NoSolutionException]
  def applyAC(variables: Array[Set[Int]], constraint: Array[Int] => Boolean): Array[Set[Int]] = {
    val sol = cartesianProduct(variables, constraint)
    if (sol.isEmpty) throw new NoSolutionException
    val result = toDomainsAC(sol)
    result
  }

  // Applying AC without pruning //

  def solutions(variables: Array[Set[Int]]): Array[Array[Int]] = {
    val currentSol: Array[Int] = Array.fill(variables.length)(0)
    val result: mutable.Set[Array[Int]] = mutable.Set()
    setIthVariable(variables, 0, currentSol, result)
    result.toArray
  }

  private def setIthVariable(variables: Array[Set[Int]], index: Int, currentSol: Array[Int], result: mutable.Set[Array[Int]]): Unit = {
    for (i <- variables(index)) {
      currentSol(index) = i
      if (index == variables.length - 1) {
        result += currentSol.clone()
      } else {
        setIthVariable(variables, index + 1, currentSol, result)
      }
    }
  }

  def applyACWithoutPruning(variables: Array[Set[Int]], constraint: Array[Int] => Boolean): Array[Set[Int]] = {
    val sols: Array[Array[Int]] = solutions(variables).filter(x => constraint(x))
    toDomainsAC(sols)
  }


  //applying BC with pruning //

  @throws[NoSolutionException]
  private def getIntervals(variables: Array[Set[Int]]): Array[Interval] = {
    variables.map(x => if (x.nonEmpty) new Interval(x) else throw new NoSolutionException)
  }

  private def intervalsToVariables(intervals: Array[Interval]): Array[Set[Int]] = {
    intervals.map(x => x.domain)
  }

  @throws[NoSolutionException]
  def applyBC(variables: Array[Set[Int]], constraint: Array[Int] => Boolean): Array[Set[Int]] = {
    val intervals = getIntervals(variables)
    var changed: Boolean = true
    while (changed) {
      changed = false
      for (i <- variables.indices) {
        val modif: Boolean = cartesianBC(intervals, constraint, i, minOrMax = true)
        val other_modif: Boolean = cartesianBC(intervals, constraint, i, minOrMax = false)
        if (modif || other_modif) changed = true
      }
    }
    intervalsToVariables(intervals)
  }

  private def reinitialize(intervals: Array[Interval]): Unit = {
    intervals.foreach(x => x.resetPos())
  }

  private def findingAcceptingValue(sol: Array[Int], interval: Interval, constraint: Array[Int] => Boolean): Boolean = {
    if (constraint(sol)) true
    else if (interval.posInInterval) {
      sol(sol.length - 1) = interval.position
      interval.incrementPos()
      findingAcceptingValue(sol, interval, constraint)
    } else {
      interval.resetPos()
      false
    }
  }

  @throws[NoSolutionException]
  private def cartesianBC(intervals: Array[Interval], constraint: Array[Int] => Boolean, id: Int, minOrMax: Boolean): Boolean = {
    reinitialize(intervals)
    val interval: Interval = intervals(id)
    var sol: Array[Int] = Array(interval.giveValue(minOrMax))
    if (intervals.length == 1) {
      if (constraint(sol))
        return false
      else {
        if (interval.domain.size == 1) throw new NoSolutionException
        interval.update(minOrMax)
        return true
      }
    }
    var i: Int = 0
    while (i < intervals.length && sol.nonEmpty) {
      if (i == id) i = i + 1
      val currentInter = intervals(i)
      if (currentInter.posInInterval) {
        sol = sol :+ currentInter.position
        currentInter.incrementPos()
        if (!findingAcceptingValue(sol, currentInter, constraint)) {
          //backtrack
          sol = sol.dropRight(2)
          i = if (i == id + 1) i - 3 else i - 2
        } else if (sol.length == intervals.length)
        //one element of the cartesian product respect the constraint.
        //Therefore, no update of the interval's min/max is required
          return false
      } else {
        //backtrack
        currentInter.resetPos()
        sol = sol.dropRight(1)
        i = if (i == id + 1) i - 3 else i - 2
      }
      i = i + 1
    }
    //no solution if we remove the last element of the domain of a variable
    if (interval.domain.size == 1) throw new NoSolutionException
    interval.update(minOrMax)
    true

  }


  /*
   * This constraint is used to test. It does absolutely nothing.
   */
  def dummyConstraint(x: Array[Set[Int]]): Array[Set[Int]] = x
}