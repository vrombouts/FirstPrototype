package checker

import scala.util.Random

class BranchOp(val domains:Array[Set[Int]]) {
  override def clone(): BranchOp ={
    new BranchOp(domains.clone())
  }
}

/**
  *domains must not contain all its sets with a size of 1
  * and neither must it have an empty set
  *
  */
class RestrictDomain(private val dom:Array[Set[Int]]) extends BranchOp(dom) {
  private val random = new Random
  var op: Int = random.nextInt(6)
  var index: Int = random.nextInt(domains.length)
  var constant: Int = randomConstant

  def randomConstant: Int = {
    var dom: Set[Int] = domains(index)
    while (dom.size<2) {
      index = random.nextInt(domains.length)
      dom=domains(index)
    }
    if (op == Op.lesserThan || op == Op.greaterThanOrEqual) dom = dom - dom.min
    else if (op == Op.greaterThan || op == Op.lesserThanOrEqual) dom = dom - dom.max
    val variable = dom.toArray
    variable(random.nextInt(variable.length))
  }

  def applyRestriction():Array[Set[Int]]={
    var domainToReduced:Set[Int]=domains(index).toArray.toSet
    for(i <- domains(index)){
      if(!Op.respectOp(op,i,constant)){ domainToReduced = domainToReduced - i}
    }
    domains(index)=domainToReduced
    domains
  }

  override def clone():RestrictDomain = {
    val r:RestrictDomain = new RestrictDomain(domains.clone())
    r.op=this.op
    r.index=this.index
    r.constant=this.constant
    r
  }

  override def toString: String = {
    val str = "Restriction of domains for" + index + "th variable with this operation :"
    str +"x_"+index+Op.printOp(op)+constant+"\n"
  }
  //type of restriction
  //which variable is touched by the restriction
  //constant
}
class Push(private val dom:Array[Set[Int]]) extends BranchOp(dom) {
  override def clone(): Push = new Push(dom.clone())
  override def toString: String = "Push\n"
}
class Pop(private val dom:Array[Set[Int]])  extends BranchOp(dom) {
  override def clone(): Pop = new Pop(dom.clone())

  override def toString: String = "Pop\n"
}
