/**
  * Created by valentin on 23/10/17.
  * This class is used to help to simulate
  * a wrong arc consistency with a
  * AllDifferent constraint
  */
class Halles(val set:Set[Int],first_id:Int) {
    var list_ids: List[Int] = List(first_id)

    def check_full:Boolean = list_ids.length == set.size
    def check_present(i:Int): Boolean = list_ids.contains(i)
    def possible_add(v:Variable): Boolean = v.domain.equals(set)
    def add(v:Variable): Unit = {
      if(!this.check_present(v.id) && !this.check_full){
        list_ids = v.id :: list_ids
      }
    }
    def propagate(variables: Array[Variable]): Boolean = {
      if(!check_full){ return false}
      var change: Boolean = false
      for(v<-variables){
        val size: Int = v.domain.size
        if (!list_ids.contains(v.id)) set.foreach(elem => v.domain.remove(elem))
        if(size!=v.domain.size) change=true
      }
      change
    }

}
