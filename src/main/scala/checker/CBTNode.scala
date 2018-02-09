package checker

class CBTNode(val b:BranchingConstraint, val nbChildren : Int){
  private var nbDescendants:Int = 0

  def update_d(n:Int) : Unit={
    nbDescendants = n
  }


  override def toString(): String ={
    return "("+b+","+nbChildren+","+nbDescendants+")"+"\n"
  }
}
