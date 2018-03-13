package checker.scheduling

class FixedActivity( val start:Int,val end:Int) {
  assert(start <= end)
  val duration: Int = end - start
}
