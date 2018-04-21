package checker.constraints

import checker._
import org.scalacheck.Prop.forAll

class Sum(operator:String, constant:Int) extends Constraint2 with ACBasic {

  def this(c:Int) = this("=", c)
  setGen()

  def setGen() : Unit = {
    gen.setNVar(10)
    val middleValue = constant/10
    if(operator.equals("=")) {
      gen.setRangeForAll(middleValue-1, middleValue+1)
      gen.setDensityForAll(0.7)
    }
    else if(operator.equals("!=")) {
      gen.setRangeForAll(middleValue-1, middleValue+1)
      gen.setDensityForAll(0.1)
      gen.setDensity(0,0.5)
    }
    else if(operator.equals(">")) {
      gen.setRangeForAll(middleValue-5, middleValue+3)
      gen.setDensityForAll(0.3)
    }
    else if(operator.equals("<")){
      gen.setRangeForAll(middleValue-2, middleValue+5)
      gen.setDensityForAll(0.3)
    }
    else if(operator.equals(">=")){
      gen.setRangeForAll(middleValue-5, middleValue+3)
      gen.setDensityForAll(0.3)
    }
    else if(operator.equals("<=")) {
      gen.setRangeForAll(middleValue-2, middleValue+5)
      gen.setDensityForAll(0.3)
    }
  }

  override def checker(solution:Array[Int]):Boolean = {
    var result : Boolean = true
    if(operator.equals("=")) result = solution.sum == constant
    else if(operator.equals("!=")) result = solution.sum != constant
    else if(operator.equals(">")) result = solution.sum > constant
    else if(operator.equals("<")) result = solution.sum < constant
    else if(operator.equals(">=")) result = solution.sum >= constant
    else if(operator.equals("<=")) result = solution.sum <= constant
    result
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
  def sumBC(variables: Array[Set[Int]]): Array[Set[Int]] = {
    var changed: Boolean = true
    val cond: (Int, Int) => Boolean = Op.condition(operator, _, _, constant)
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
    variables
  }

  override def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]] = sumBC(variables)

}
