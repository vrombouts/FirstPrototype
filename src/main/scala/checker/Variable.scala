package checker



/**
  * Created by valentin on 13/10/17.
  */
object global{
  var id = 0
}

class Variable(val dom:Set[Int],val id:Int){
  var domain = collection.mutable.SortedSet(dom.toList:_*)
  def this(domain:Set[Int])= {
    this(domain, global.id)
    global.id = global.id + 1
  }

  override def toString: String = "Variable "+id+": "+domain.toSet.toString()+"\n"
  def compare_domain(variable:Variable): Boolean =this.domain.size < variable.domain.size
  def compare_id(variable: Variable):Boolean= this.id < variable.id
  def same_domain(variable: Variable): Boolean = variable.domain.equals(this.domain)
}
