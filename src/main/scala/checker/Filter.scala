package checker

abstract class Filter {
  def filter(variables: Array[Set[Int]]): Array[Set[Int]]
}
