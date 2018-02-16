package checker


/*
 * This class contains all the functions used specifically to check the unary resource constraint
 *
 */
class UnaryResource {

  def overloadChecking(activities:Array[Activity]): Boolean = {
    val ectMap: Map[Array[Activity], Int] = Map()
    val sortedActivities=activities.sortWith(_.lct< _.lct)
    var theta:Array[Activity]=Array()
    sortedActivities.foreach{activity =>
      theta = theta :+ activity
      if(earliestCompletionTime(theta, ectMap)> activity.lct)
        false
    }
    true
  }

  def earliestCompletionTime(activities: Array[Activity],ectMap:Map[Array[Activity],Int]): Int = {
    if(ectMap.keySet.contains(activities)) ectMap(activities)
    var max = Integer.MIN_VALUE
    activities.foreach{ x =>
      val ect = earliestCompletionTime(activities.filterNot(y => y==x),ectMap)
      if(max< ect) max=ect
    }
    val estPlusDuration = ectFalse(activities)
    ectMap(activities)=if(estPlusDuration>max) estPlusDuration else max
    ectMap(activities)
  }

  def ectFalse(activities: Array[Activity]): Int = {
    var min = Integer.MAX_VALUE
    var duration = 0
    activities.foreach{x =>if(min>x.est) min = x.est; duration += x.dur}
    min+duration
  }
}
