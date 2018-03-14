package checker.constraints

import checker._
import org.scalacheck.Prop.forAll

object Sum extends Checker {

  //private var constant:Int=0
  private var operator: String = Op.equal

  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = {
    sumBC(variables, operator)
  }

  /*def checkSum(constraint:Array[Set[Int]]=>Array[Set[Int]],constant:Int, operation:Int):Unit = {

    forAll(Generators.basic){ x =>
      x.isEmpty || checkEmpty(x) || checkConstraint(x.toArray,constraint)
    }.check
  }*/

  def checkBC(constraint: Array[Set[Int]] => Array[Set[Int]], operation: String): Unit = {
    operator = operation
    val check: (Array[Set[Int]]) => Boolean = checkConstraint(_, constraint)
    forAll(Generators.sum) { x =>
      if (x._1.isEmpty || checkEmpty(x._1)) true
      else {
        val array = x._1.toArray
        check(array ++ Array(Set(x._2)))
      }
    }.check
    LimitCases.sumLimitCases.foreach { limitCase => check(limitCase) }
  }

  def sum(constant: Int, operation: String, nbVar: Int): Array[Int] => Boolean = sum(constant, operation, nbVar, _)

  def sum(constant: Int, operation: String, nbVar: Int, solution: Array[Int]): Boolean = {
    if (solution.length < nbVar - 1) return true
    var sum: Int = 0
    solution.foreach(x => sum += x)
    Op.respectOp(operation, sum, constant)
  }

  def addWithoutOverflow(sum: Int, value: Int): Int = {
    if (value < 0) {
      if (Integer.MIN_VALUE - value > sum) Integer.MIN_VALUE
      else sum + value
    } else {
      if (Integer.MAX_VALUE - value < sum) Integer.MAX_VALUE
      else sum + value
    }
  }

  @throws[NoSolutionException]
  def sumBC(vars: Array[Set[Int]], operation: String): Array[Set[Int]] = {
    val constant: Int = vars.last.last
    val variables = vars.dropRight(1)
    var changed: Boolean = true
    val cond: (Int, Int) => Boolean = Op.condition(operation, _, _, constant)
    while (changed) {
      changed = false
      for (i <- variables.indices) {
        if (variables(i).isEmpty)
          throw new NoSolutionException
        val min: Int = variables(i).min
        val max: Int = variables(i).max
        var sMin: Int = -min
        var sMax: Int = -max
        variables.foreach(x => {
          sMin = addWithoutOverflow(sMin, x.min)
          sMax = addWithoutOverflow(sMax, x.max)
        })

        //Sum is BC so check min and max values
        if (cond(addWithoutOverflow(sMin, min), addWithoutOverflow(sMax, min))) {
          variables(i) = variables(i) - min
          if (variables(i).isEmpty) throw new NoSolutionException
          changed = true
        }
        if (cond(addWithoutOverflow(sMin, max), addWithoutOverflow(sMax, max))) {
          variables(i) = variables(i) - max
          if (variables(i).isEmpty) throw new NoSolutionException
          changed = true
        }
      }
    }
    variables :+ Set(constant)
  }
}
