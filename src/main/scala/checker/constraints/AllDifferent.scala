package checker.constraints
import checker._

object AllDifferent extends Checker{

  private var isAC:Boolean = true
  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    if(isAC) Constraint.applyAC(variables,allDifferent)
    else Constraint.applyBC(variables,allDifferent)
  }

  override def applyConstraint(b:BranchOp):Array[Set[Int]] = null
  /*
  * This function checks if the constraint passed in argument apply correctly an
  * allDifferent constraint with arc consistency.
  */
  def checkAC( constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    check(isAc = true,constraint)
  }
  def checkBC( constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    check(isAc = false,constraint)
  }
  private def check(isAc: Boolean, constraint:Array[Set[Int]]=>Array[Set[Int]]): Unit = {
    isAC=isAc
    if(isAc) {
      Constraint.checkAC(constraint,allDifferent)
    }
    else Constraint.checkBC(constraint,allDifferent)
    val checkAllDiff: Array[Set[Int]] => Boolean = checkConstraint(_,constraint)
    LimitCases.allDifferentLimitCases.foreach(x => checkAllDiff(x))
    println("All tests executed.")
  }

  def allDifferent(solution: Array[Int]):Boolean = {
    solution.toSet.size==solution.length
  }

}
