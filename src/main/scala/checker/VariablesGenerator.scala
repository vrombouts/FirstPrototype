package checker

import org.scalacheck.Test.TestCallback
import org.scalacheck.{Gen, Test}

import scala.util.Random

/*
 * generator of variables represented as Array[Set[Int]].
 * Basic nbVars: 5 variables
 * Basic density: 0.2 for each variable
 * Basic range: [-10,10] for each variable
 */
class VariablesGenerator {


  private[this] var nbVars: Int = 5
  private[this] var densities: Array[Double] = Array.fill(nbVars)(4.0 / 20.0)
  private[this] var ranges: Array[(Int, Int)] = Array.fill(nbVars)((-10, 10))
  var baseRange: (Int, Int) = (-10, 10)
  var baseDensity: Double = 4.0 / 20.0
  val random = new Random()
  private[this] var seed: Option[Long] = None
  private[this] var nbTests: Option[Int] = None

  private[checker] class testParams(seed: Long, nbTests: Int) extends Test.Parameters {
    def this(seed: Long) = this(seed, 100)

    def this(nbTests: Int) = this(random.nextLong, nbTests)

    def this() = this(random.nextLong, 100)

    override val minSuccessfulTests: Int = nbTests
    val minSize: Int = 0
    val maxSize: Int = Gen.Parameters.default.size
    override val rng: scala.util.Random = new Random(seed)
    val workers: Int = 1
    val testCallback: TestCallback = new TestCallback {}
    val maxDiscardRatio: Float = 5
    val customClassLoader: Option[ClassLoader] = None
  }

  def setSeed(sd: Long): Unit = {
    seed = Some(sd)
    random.setSeed(sd)
  }

  def getSeed: Long = seed match {
    case Some(sd) => sd
    case None => random.nextLong
  }

  def randomSeed(): Unit = {
    seed = None
    random.setSeed(new Random().nextLong)
  }

  def setNbTests(n: Int): Unit = nbTests = Some(n)

  def getTestParameters: testParams = {
    (seed, nbTests) match {
      case (Some(l), Some(t)) => new testParams(l, t)
      case (Some(l), None) => new testParams(l)
      case (None, Some(t)) => new testParams(t)
      case _ => new testParams
    }
  }

  def gen: Gen[List[Set[Int]]] =
    for {
      seq <- Gen.sequence(genList)
    } yield seq.toArray(new Array[Set[Int]](seq.size())).toList

  private[this] def genList: List[Gen[Set[Int]]] = {
    var l: List[Gen[Set[Int]]] = List()
    for (i <- nbVars - 1 to 0 by -1) {
      l = genVar(i) :: l
    }
    l
  }

  private[this] def genVar(i: Int): Gen[Set[Int]] = {
    val min = ranges(i)._1
    val max = ranges(i)._2
    val dif = max - min
    val size = Math.max(Math.round((dif + 1) * densities(i)), 1).toInt
    Gen.containerOfN[Set, Int](size, Gen.choose(min, max))
  }

  def setNVar(n: Int): Unit = {
    nbVars = if (n < 1) 1 else n
    densities = Array.fill(nbVars)(baseDensity)
    ranges = Array.fill(nbVars)(baseRange)
  }

  def getNVar(): Int = nbVars

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
  def addNVar(n: Int): Unit = {
    for (_ <- 1 to n) addVar()
  }

  /* This function add n variables to the instances generated
   * with the given density and range
   */
  def addNVar(n: Int, density: Double, range: (Int, Int)): Unit = {
    for (_ <- 1 to n) addVar(density, range)
  }

  def setDensity(index: Int, density: Double): Unit = {
    var d = densities(index)
    if (density <= 1.0 && density >= 0.0)
      d = density
    densities(index) = d
  }

  def setDensityForAll(density: Double): Unit = {
    if (density <= 1.0 && density >= 0.0)
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
    densities = densities.filterNot { _ => j += 1; j == index }
    j = -1
    ranges = ranges.filterNot { _ => j += 1; j == index }
  }

  override def toString: String = {
    var str = "Generator :\n\tSeed: "
    seed match {
      case Some(sd) => str += sd
      case None => str += "random"
    }
    str += "\n" +
      "\tvariable\tdensity\trange\n"
    for (i <- 0 until nbVars) str += "\t" + i + "\t\t\t" + densities(i) + "\t\t" + ranges(i) + "\n"
    str
  }

}