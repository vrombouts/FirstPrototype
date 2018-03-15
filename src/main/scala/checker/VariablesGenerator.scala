package checker

import org.scalacheck.Gen

/*
 * generator of variables represented as Array[Set[Int]].
 * Basic nbVars: 5 variables
 * Basic density: 0.8 for each variable
 * Basic range: [-10,10] for each variable
 */
class VariablesGenerator {
  private[this] var nbVars: Int = 5
  private[this] var densities: Array[Double] = Array.fill(nbVars)(4.0 / 20.0)
  private[this] var ranges: Array[(Int, Int)] = Array.fill(nbVars)((-10, 10))
  var baseRange: (Int, Int) = (-10, 10)
  var baseDensity: Double = 4.0 / 20.0

  def gen: Gen[List[Set[Int]]] = for {
    seq <- Gen.sequence(GenList)
  } yield seq.toArray(new Array[Set[Int]](seq.size())).toList

  private def GenList: List[Gen[Set[Int]]] = {
    var l: List[Gen[Set[Int]]] = List()
    for (i <- nbVars - 1 to 0 by -1) {
      l = GenVar(i) :: l
    }
    l
  }
  private def GenVar(i: Int): Gen[Set[Int]] = {
    val min = ranges(i)._1
    val max = ranges(i)._2
    val dif = max - min
    val size: Double = dif * densities(i)
    Gen.containerOfN[Set, Int](size.toInt, Gen.choose(min, max))
  }

  def setNVar(n:Int):Unit= {
    nbVars = n
    densities = Array.fill(nbVars)(baseDensity)
    ranges = Array.fill(nbVars)(baseRange)
  }

  def addVar(): Unit = {
    nbVars += 1
    densities = densities :+ baseDensity
    ranges = ranges :+ baseRange
  }

  def addVar(density: Double, range: (Int, Int)): Unit = {
    nbVars += 1
    densities = densities :+ density
    ranges = ranges :+ range
  }

  /* This function add n variables to the instances generated
   * with the basic density and range
   */
  def AddNVar(n: Int): Unit = {
    for (_ <- 1 to n) addVar()
  }

  /* This function add n variables to the instances generated
   * with the given density and range
   */
  def AddNVar(density: Double, range: (Int, Int), n: Int): Unit = {
    for (_ <- 1 to n) addVar(density, range)
  }

  def setDensity(index: Int, density: Double): Unit = densities(index) = density

  def setDensityForAll(density: Double): Unit = {
    baseDensity = density
    densities = Array.fill(nbVars)(baseDensity)
  }

  def setRange(index: Int, range: (Int, Int)): Unit = ranges(index) = range

  def setRangeForAll(range: (Int, Int)): Unit = {
    baseRange = range
    ranges = Array.fill(nbVars)(baseRange)
  }

  def remove(index: Int): Unit = {
    if (index < 0 || index >= nbVars) return
    nbVars -= 1
    var j = -1
    densities = densities.filter { _ => j += 1; j == index }
    j = -1
    ranges = ranges.filter { _ => j += 1; j == index }
  }

  override def toString: String = {
    var str = "vars\tdensity\trange\n"
    for(i <- 0 until nbVars) str +=i+"\t\t"+densities(i)+"\t\t"+ranges(i)+"\n"
    str
  }

}
