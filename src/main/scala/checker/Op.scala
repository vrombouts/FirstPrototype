package checker

object Op {
  val equal=0
  val different=1
  val lesserThan=2
  val lesserThanOrEqual=3
  val greaterThan=4
  val greaterThanOrEqual=5

  def opposite(operation: Int): Int={
    operation match{
      case Op.equal => Op.different
      case Op.different => Op.equal
      case Op.lesserThan => Op.greaterThanOrEqual
      case Op.lesserThanOrEqual => greaterThan
      case Op.greaterThan => Op.lesserThanOrEqual
      case Op.greaterThanOrEqual => Op.lesserThan
      case _ => -1 //no such op
    }
  }

  def respectOp(operation:Int, sum:Int, constant:Int) :Boolean= {
    operation match {
      case Op.equal => sum == constant
      case Op.different => sum != constant
      case Op.lesserThan => sum < constant
      case Op.lesserThanOrEqual => sum <= constant
      case Op.greaterThan => sum > constant
      case Op.greaterThanOrEqual => sum >= constant
      case _ => sum == constant
    }
  }

  def condition(operation:Int, s_min:Int, s_max:Int, constant:Int, value:Int) : Boolean = {
    operation match {
      case Op.equal => Op.respectOp(Op.lesserThan, value + s_min, constant) && Op.respectOp(Op.greaterThan, value + s_max, constant)
      case Op.different => !(s_max==s_min && s_min+value==constant)
      case Op.lesserThan => Op.respectOp(Op.opposite(operation), value + s_min, constant)
      case Op.lesserThanOrEqual => Op.respectOp(Op.opposite(operation), value + s_min, constant)
      case Op.greaterThan => Op.respectOp(Op.opposite(operation), value + s_max, constant)
      case Op.greaterThanOrEqual => Op.respectOp(Op.opposite(operation), value + s_max, constant)
      case _ => Op.respectOp(Op.lesserThan, value + s_min, constant) && Op.respectOp(Op.greaterThan, value + s_max, constant)
    }
  }
}
