package checker

object Op {
  val equal=0
  val different=1
  val lesserThan=2
  val lesserThanOrEqual=3
  val greaterThan=4
  val greaterThanOrEqual=5
  
  def Opposite(operation: Int): Int={
    operation match{
      case Op.equal => Op.different
      case Op.different => Op.equal
      case Op.lesserThan => Op.greaterThanOrEqual
      case Op.lesserThanOrEqual => Op.greaterThan
      case Op.greaterThan => Op.lesserThanOrEqual
      case Op.greaterThanOrEqual => Op.lesserThan
    }
  }
}
