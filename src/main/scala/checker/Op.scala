package checker

import scala.util.Random

object Op {
  val equal = "="
  val different = "!="
  val lesserThan = "<"
  val lesserThanOrEqual = "<="
  val greaterThan = ">"
  val greaterThanOrEqual = ">="

  def opposite(operation: String): String = {
    operation match {
      case `equal` => different
      case `different` => equal
      case `lesserThan` => greaterThanOrEqual
      case `lesserThanOrEqual` => greaterThan
      case `greaterThan` => lesserThanOrEqual
      case `greaterThanOrEqual` => lesserThan
      case _ => "no such op" //no such op
    }
  }

  def respectOp(operation: String, sum: Int, constant: Int): Boolean = {
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

  def condition(operation: String, sMin: Int, sMax: Int, constant: Int): Boolean = {
    operation match {
      case `equal` => respectOp(greaterThan, sMin, constant) || respectOp(lesserThan, sMax, constant)
      case `different` => sMax == sMin && sMin == constant
      case `lesserThan` => respectOp(opposite(operation), sMin, constant)
      case `lesserThanOrEqual` => respectOp(opposite(operation), sMin, constant)
      case `greaterThan` => respectOp(opposite(operation), sMax, constant)
      case `greaterThanOrEqual` => respectOp(opposite(operation), sMax, constant)
      case _ => respectOp(lesserThan, sMin, constant) && respectOp(greaterThan, sMax, constant)
    }
  }

  def randomOp(rd:Random = new Random()): String = {
    val rand = rd
    rand.nextInt(6) match {
      case 0 => equal
      case 1 => different
      case 2 => lesserThan
      case 3 => lesserThanOrEqual
      case 4 => greaterThan
      case 5 => greaterThanOrEqual
    }
  }
}
