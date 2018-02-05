package checker

/**
  * Created by aumassart on 31.01.18.
  */
class Interval(var domain:Set[Int]) {

  var min:Int=domain.min
  var max:Int=domain.max
  var pos:Int=min

  def update_min(): Unit ={
    domain = domain - min
    min=domain.min
  }

  def update_max(): Unit ={
    domain = domain - max
    max=domain.max
  }

  def update(minOrMax:Boolean): Unit ={
    if(minOrMax) update_min()
    else update_max()
  }

  def giveValue(minOrMax : Boolean): Int ={
    if(minOrMax) return min
    else return max
  }

}
