package checker


/**
  * Created by valentin on 02/11/17.
  */
trait JavaCheck {
    def check_allDifferent(): Unit
    def Constraint(tab: java.util.ArrayList[java.util.Set[java.lang.Integer]]): java.util.ArrayList[java.util.Set[java.lang.Integer]]
}
