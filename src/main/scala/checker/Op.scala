package checker

import scala.util.Random

/**
  * this object represent the simple operations supported by CPChecker
  * for RestrictDomain objects.
  */
object Op {
  val equal = "="
  val different = "!="
  val lesserThan = "<"
  val lesserThanOrEqual = "<="
  val greaterThan = ">"
  val greaterThanOrEqual = ">="

  /**
    * @param operation : an operation represented as a String
    * @return the opposite operation. (for example: '<' gives '>=')
    */
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

  /**
    * @param operation : an operation represented as a String
    * @param sum       : an integer
    * @param constant  : another integer
    * @return sum operation constant.
    */
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

  /**
    * @param rd : a Random scala object
    * @return use 'rd' to return randomly one of the supported
    *         operations.
    */
  def randomOp(rd: Random = new Random()): String = {
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
