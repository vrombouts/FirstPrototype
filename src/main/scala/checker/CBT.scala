package checker

class CBT {
  var nVisitedNodes = 0
  var S: Array[CBTNode] = Array()

  def generate(domains: Array[Set[Int]], branchingProc: BranchingProcedure): Unit = {
    nVisitedNodes = 0
    visit(domains, branchingProc, new BranchingConstraint())
  }

  def isSolved(domains: Array[Set[Int]]): Boolean = {
    domains.count(x => x.size != 1) == 0
  }

  def visit(domains: Array[Set[Int]], branchingProc: BranchingProcedure, b: BranchingConstraint): Unit = {
    nVisitedNodes = nVisitedNodes + 1
    if (!b.applyOn(domains) || isSolved(domains)) S = S :+ new CBTNode(b, 0)
    else {
      var branchingCtrs: List[BranchingConstraint] = branchingProc.branch(domains)
      var nodeIndex: Int = nVisitedNodes
      var c = branchingCtrs.length
      S = S :+ new CBTNode(b, c)
      for (i <- 1 to c) {
        val domainCopy = domains.clone()
        visit(domainCopy, branchingProc, branchingCtrs(i))
      }
      S(nodeIndex).update_d(nVisitedNodes - nodeIndex)
    }
  }

  override def toString(): String ={
    var str=""
    for(i <- S.indices){
      str+="[ " + S(i).toString()+" ]"+"\n"
    }
    return str
  }

}

object Main{
  def main(args: Array[String]): Unit ={
    var cbt:CBT = new CBT()
    cbt.generate(Array(Set(0,1,2), Set(0,5)), new BranchingProcedure(1,2,Op.equal))
    println(cbt.toString())
  }
}
