package checker

/**
  * This class is used to help to simulate
  * a wrong arc consistency with a
  * AllDifferent constraint
  */
class Halles(val set:Set[Int],firstID:Int) {
    var listIDs: List[Int] = List(firstID)

    def checkFull:Boolean = listIDs.length == set.size
    def checkPresent(i:Int): Boolean = listIDs.contains(i)
    def possibleAdd(v:Variable): Boolean = v.domain.equals(set)
    def add(v:Variable): Unit = {
      if(!this.checkPresent(v.id) && !this.checkFull){
        listIDs = v.id :: listIDs
      }
    }
    def propagate(variables: Array[Variable]): Boolean = {
      if(!checkFull){ return false}
      var change: Boolean = false
      for(v<-variables){
        val size: Int = v.domain.size
        if (!listIDs.contains(v.id)) set.foreach(elem => v.domain.remove(elem))
        if(size!=v.domain.size) change=true
      }
      change
    }

}
