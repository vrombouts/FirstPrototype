package checker

/**
  * This class is used to help to simulate
  * a wrong arc consistency with a
  * AllDifferent constraint
  */
class Halles(val set:Set[Int],first_id:Int) {
    var list_ids: List[Int] = List(first_id)

    def checkFull:Boolean = list_ids.length == set.size
    def checkPresent(i:Int): Boolean = list_ids.contains(i)
    def possibleAdd(v:Variable): Boolean = v.domain.equals(set)
    def add(v:Variable): Unit = {
      if(!this.checkPresent(v.id) && !this.checkFull){
        list_ids = v.id :: list_ids
      }
    }
    def propagate(variables: Array[Variable]): Boolean = {
      if(!checkFull){ return false}
      var change: Boolean = false
      for(v<-variables){
        val size: Int = v.domain.size
        if (!list_ids.contains(v.id)) set.foreach(elem => v.domain.remove(elem))
        if(size!=v.domain.size) change=true
      }
      change
    }

}
