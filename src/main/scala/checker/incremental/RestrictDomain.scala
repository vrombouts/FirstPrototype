package checker.incremental

import checker.Op
import scala.util.Random

/**
  * branching operation representing the reduction of one
  * of the domains of 'doms'. This reduction is done by a
  * constraint in the format : "doms(index) op constant".
  * 'op' being a string representing a relation between
  * the 'constant' and the values of the domain. This co-
  * nstraint is randomly created thanks to the 'random'
  * object of its constructor.
  *
  * @param doms   : array of domains before the restrict-
  *               ion of domains.
  * @param random : Random object used to create the con-
  *               straint this object defines.
  */
class RestrictDomain(val doms: Array[Set[Int]], val random: Random) extends BranchOp(doms) {
  var index: Int = getIndex
  var op: String = Op.randomOp(random)
  var constant: Int = randomConstant

  private def randomConstant: Int = {
    var dom = domains(index)
    if (op.equals(Op.lesserThan) || op.equals(Op.greaterThanOrEqual)) dom = dom - dom.min
    else if (op.equals(Op.greaterThan) || op.equals(Op.lesserThanOrEqual)) dom = dom - dom.max
    val variable = dom.toArray
    variable(random.nextInt(variable.length))
  }

  private def getIndex: Int = {
    var possibleIndexes: List[Int] = List()
    for (i <- domains.indices) {
      if (domains(i).size > 1) possibleIndexes = i :: possibleIndexes
    }
    val indexOfIndex = random.nextInt(possibleIndexes.size)
    possibleIndexes(indexOfIndex)
  }

  /**
    * After a call of this function, the 'domains' field of this
    * branching operation is set to the return value.
    *
    * @return the domains after the application of this branching
    *         operation.
    */
  def applyRestriction: Array[Set[Int]] = {
    domains(index) = domains(index).filter(value => Op.respectOp(op, value, constant))
    domains
  }

  override def clone: BranchOp = {
    val rd = new RestrictDomain(domains.clone, random)
    rd.index = index
    rd.constant = constant
    rd.op = op
    rd
  }

  override def toString: String = "Restriction of domains (x_" + index + op + constant + ")"
}
