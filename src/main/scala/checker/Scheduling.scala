package checker

object Scheduling {
  def overloadChecking(activities: Array[Activity]): Boolean = {
    omegas(activities).foreach{ set =>
      if( est(set) + p(set) > lct(set)) return false
    }
    true
  }

  def notFirst(activities: Array[Activity]): Array[Activity] = {
    omegas(activities).foreach{ set => for(activity <- activities){
      if(!set.contains(activity) && activity.ect > lct(set)-p(set))
        activity.estReduce(minEct(set))
    }
    }
    activities
  }

  def notLast(activities: Array[Activity]): Array[Activity] = {
    omegas(activities).foreach{ set => for(activity <- activities){
        if(!set.contains(activity) && est(set) + p(set)>activity.lst)
            activity.lctReduce(maxLst(set))
      }
    }
    activities
  }










  //TODO: Think if Stream would not be more efficient
  private def omegas(activities: Array[Activity]): List[Array[Activity]] = {
    def constructList(omegas: List[Array[Activity]], index:Int): List[Array[Activity]] = {
      var newOmegas: List[Array[Activity]] = omegas
      omegas.foreach{x => newOmegas = (activities(index)+:x) :: newOmegas}
      if(index>0) return constructList(newOmegas, index-1)
      newOmegas
    }
    val omegas: List[Array[Activity]] = List(Array())
    constructList(omegas,activities.length-1)
  }

  private def est(activities: Array[Activity]): Int = {
    if(activities.isEmpty) return Integer.MIN_VALUE
    var min = Integer.MAX_VALUE
    activities.foreach{act => if( act.est<min) min = act.est}
    min
  }
  private def p(activities: Array[Activity])  : Int = {
    if(activities.isEmpty) return 0
    var p = 0
    activities.foreach{act => p += act.dur}
    p
  }
  private def lct(activities: Array[Activity]): Int = {
    if (activities.isEmpty) return Integer.MAX_VALUE
    var max = Integer.MIN_VALUE
    activities.foreach { act => if (act.lct > max) max = act.lct }
    max
  }
  private def maxLst(activities: Array[Activity]): Int = {
    if (activities.isEmpty) return Integer.MAX_VALUE
    var max = Integer.MIN_VALUE
    activities.foreach { act => if (act.lst > max) max = act.lst }
    max
  }
  private def minEct(activities: Array[Activity]): Int = {
    if (activities.isEmpty) return Integer.MIN_VALUE
    var min = Integer.MAX_VALUE
    activities.foreach { act => if (act.ect < min) min = act.ect }
    min
  }
}
