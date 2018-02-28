package checker

object Op {
  val equal=0
  val different=1
  val lesserThan=2
  val lesserThanOrEqual=3
  val greaterThan:Int=4
  val greaterThanOrEqual=5

  def opposite(operation: Int): Int={
    operation match{
      case `equal` => Op.different
      case `different` => Op.equal
      case `lesserThan` => Op.greaterThanOrEqual
      case `lesserThanOrEqual` => greaterThan
      case `greaterThan` => Op.lesserThanOrEqual
      case `greaterThanOrEqual` => Op.lesserThan
      case _ => -1 //no such op
    }
  }

  def printOp(operation:Int):String = {
    operation match{
      case `equal`              => "=="
      case `different`          => "!="
      case `lesserThan`         => "<"
      case `lesserThanOrEqual`  => "<="
      case `greaterThan`        => ">"
      case `greaterThanOrEqual` => ">="
      case _ => "NoSuchOp" //no such op
    }
  }

  def respectOp(operation:Int, sum:Int, constant:Int) :Boolean= {
    operation match {
      case `equal` => sum == constant
      case `different` => sum != constant
      case `lesserThan` => sum < constant
      case `lesserThanOrEqual` => sum <= constant
      case `greaterThan` => sum > constant
      case `greaterThanOrEqual` => sum >= constant
      case _ => sum == constant
    }
  }

  def condition(operation:Int, sMin:Int, sMax:Int, constant:Int) : Boolean = {
    operation match {
      case `equal` => respectOp(greaterThan,sMin, constant) || respectOp(lesserThan, sMax, constant)
      case `different` => sMax==sMin && sMin==constant
      case `lesserThan` => respectOp(opposite(operation), sMin, constant)
      case `lesserThanOrEqual` => respectOp(opposite(operation), sMin, constant)
      case `greaterThan` => respectOp(opposite(operation), sMax, constant)
      case `greaterThanOrEqual` => respectOp(opposite(operation), sMax, constant)
      case _ => respectOp(lesserThan, sMin, constant) && respectOp(greaterThan, sMax, constant)
    }
  }
}
