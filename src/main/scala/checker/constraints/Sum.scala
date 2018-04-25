package checker.constraints

import checker._

class Sum(operator:String, constant:Int) extends Constraint with ACBasic {

  def this(c:Int) = this("=", c)
  setGen(10)

  def setGen(nbVars:Int) : Unit = {
    gen.reset()
    gen.setNVar(nbVars)
    val middleValue = constant/nbVars
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
    if(operator.equals("=")) return solution.sum == constant
    else if(operator.equals("!=")) return solution.sum != constant
    else if(operator.equals(">")) return solution.sum > constant
    else if(operator.equals("<")) return solution.sum < constant
    else if(operator.equals(">=")) return solution.sum >= constant
    else if(operator.equals("<=")) return solution.sum <= constant
    true
  }


  override def limitCases(): Array[Array[Set[Int]]] = {
    Array(
      //Array(Set(Integer.MAX_VALUE), Set(1), Set(Integer.MIN_VALUE)),
      //Array(Set(Integer.MIN_VALUE), Set(-1),Set(Integer.MAX_VALUE)),
      Array(Set(2), Set(constant-2)),
      Array(Set(2, constant - 2), Set(2, constant - 2)),
      Array(Set(1, 2), Set(2, 4), Set(constant - 6)), // constant == sMax
      Array(Set(1, 2), Set(2, 4), Set(constant - 5)), // constant > sMax
      Array(Set(1, 2), Set(2, 4), Set(constant - 7)), // constant < sMax
      Array(Set(1, 2), Set(2, 4), Set(constant - 3)), // constant == sMin
      Array(Set(1, 2), Set(2, 4), Set(constant - 2)), // constant > sMin
      Array(Set(1, 2), Set(2, 4), Set(constant - 4)), // constant < sMin
      Array(Set(2), Set(2), Set(constant - 4)) // constant == sMin == sMax
        //Array(Set(0), Set(0), Set(-3), Set(-5,0,4), Set(8,4,5,1))
    )
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
