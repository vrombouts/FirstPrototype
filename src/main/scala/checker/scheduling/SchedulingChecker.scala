package checker.scheduling

import checker.NoSolutionException

trait SchedulingChecker {

  private def clone(activities: Array[Activity]): Array[Activity] =
    activities.map(x => new Activity(x.start, x.duration, x.end))

  def checkScheduling(activities: Array[Activity],
                      constraintTested: Array[Activity] => Array[Activity],
                      ourConstraint: Array[Activity] => Array[Activity]): Boolean = {
    var ourReducedDomains: Array[Activity] = Array()
    var theirReducedDomains: Array[Activity] = Array()
    var ourError = false
    var theirError = false
    try {
      ourReducedDomains = ourConstraint(clone(activities))
    } catch {
      case _: NoSolutionException => ourError = true
    }
    try {
      theirReducedDomains = constraintTested(activities)
    } catch {
      case _: NoSolutionException => theirError = true
    }
    if (ourError && theirError) return true
    if (ourError && !theirError) {
      for (activity <- theirReducedDomains) {
        if (activity.isEmpty)
          return true
      }
      println("Failed for " + activities.toList)
      println("You should return an exception")
      println("but you have " + theirReducedDomains.toList)
      return false
    }
    if (!ourError && theirError) {
      for (activity <- ourReducedDomains) {
        if (activity.isEmpty)
          return true
      }
      println("Failed for " + activities.toList)
      println("You shoud have " + ourReducedDomains.toList)
      println("But you returned an exception")
      return false
    }
    if (ourReducedDomains.length != theirReducedDomains.length) {
      println("You don't return the correct number of activities in your solution!")
      return false
    }
    for (i <- ourReducedDomains.indices) {
      if (!ourReducedDomains(i).equals(theirReducedDomains(i))) {
        println("Failed for " + activities.toList)
        println("You shoud have " + ourReducedDomains.toList)
        println("But you had " + theirReducedDomains.toList)
        return false
      }
    }
    true
  }
}
