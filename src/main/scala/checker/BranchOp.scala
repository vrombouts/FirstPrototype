package checker

import  scala.util.Random

abstract class BranchOp {}

/**
  *domains must not contain all its sets with a size of 1
  * and neither must it have an empty set
  *
  */
class RestrictDomain(private val domains:Array[Set[Int]]) extends BranchOp {
  private[this] val random = new Random
  val op: Int = random.nextInt(6)
  var index: Int = random.nextInt(domains.length)
  val constant: Int = randomConstant

  def randomConstant: Int = {
    var dom: Set[Int] = domains(index)
    while (dom.size<2) {
      index = random.nextInt(domains.length)
      dom=domains(index)
    }
    if (op == Op.lesserThan || op == Op.greaterThanOrEqual) dom = dom - dom.min
    else if (op == Op.greaterThan || op == Op.lesserThanOrEqual) dom = dom - dom.max
    val variable = domains(index).toArray
    variable(random.nextInt(variable.length))
  }

  //type of restriction
  //which variable is touched by the restriction
  //constant
}
class Push extends BranchOp {}
class Pop  extends BranchOp {}
