package checker

import scala.collection.mutable


/*
 * This class contains all the functions used specifically to check the unary resource constraint
 *
 */
class UnaryResource {

  def overloadChecking(activities:Array[Activity]): Boolean = {
    val ectMap: mutable.Map[Array[Activity], Int] = mutable.Map[Array[Activity],Int]()
    val sortedActivities=activities.sortWith(_.lct< _.lct)
    var theta:Array[Activity]=Array()
    sortedActivities.foreach{activity =>
      theta = theta :+ activity
      if(earliestCompletionTime(theta, ectMap)> activity.lct)
        return false
    }
    true
  }

  def earliestCompletionTime(activities: Array[Activity],ectMap:mutable.Map[Array[Activity],Int]): Int = {
    if(ectMap.keySet.contains(activities)) ectMap(activities)
    var max = Integer.MIN_VALUE
    activities.foreach{x =>
      val act=activities.filterNot(y => y==x)
      if(act.nonEmpty) {
        val ect = earliestCompletionTime(act,ectMap)
        if(max< ect) max=ect
      }
    }
    val estPlusDuration = sumOfEstAndDuration(activities)
    ectMap(activities)=if(estPlusDuration>max) estPlusDuration else max
    return ectMap(activities)
  }

  def sumOfEstAndDuration(activities: Array[Activity]): Int = {
    var min = Integer.MAX_VALUE
    var duration = 0
    activities.foreach{x =>if(min>x.est) min = x.est; duration += x.dur}
    min+duration
  }
}
