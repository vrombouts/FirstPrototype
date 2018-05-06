package checker

import org.scalacheck.Test.Parameters
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Test}

import scala.util.Random

/*
 * generator of variables represented as Array[Set[Int]].
 * Basic nbVars: 5 variables
 * Basic density: 0.2 for each variable
 * Basic range: [-10,10] for each variable
 */
class TestArgs {

  var nbDive = 25
  private[this] var nbVars: Int = 5
  private[this] var densities: Array[Double] = Array.fill(nbVars)(4.0 / 20.0)
  private[this] var ranges: Array[(Int, Int)] = Array.fill(nbVars)((-10, 10))
  var baseRange: (Int, Int) = (-10, 10)
  var baseDensity: Double = 4.0 / 20.0
  val random: Random = new Random()
  private[this] var seed: Option[Long] = None
  private[this] var nbTests: Option[Int] = None

  private[this] var param: Parameters = Test.Parameters.default

  def setSeed(sd: Long): Unit = {
    param=param.withInitialSeed(sd)
    seed = Some(sd)
  }

  def getSeed: Long = seed match {
    case Some(sd) => sd
    case None => random.nextLong
  }

  def randomSeed(): Unit = {
    seed = None
    random.setSeed(new Random().nextLong)
  }

  def getNbVars: Int = nbVars

  def setNbTests(n: Int): Unit = {
    nbTests = Some(n)
    param = param.withMinSuccessfulTests(n)
  }

  def getNbTests: Int = nbTests match {
    case Some(n) => n
    case _ => 100
  }

  def getTestParameters: Parameters = {
    param
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

  def addVar(density: Double, min: Int, max: Int): Unit = {
    nbVars += 1
    densities = densities :+ density
    ranges = ranges :+ (min, max)
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

  /* This function add n variables to the instances generated
 * with the given density and range
 */
  def addNVar(n: Int, density: Double, min: Int, max: Int): Unit = {
    for (_ <- 1 to n) addVar(density, (min, max))
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

  def setRange(index: Int, min: Int, max: Int): Unit = ranges(index) = (min, max)

  def setRangeForAll(min: Int, max: Int): Unit = {
    baseRange = (min, max)
    ranges = Array.fill(nbVars)(baseRange)
  }

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

  def reset(): Unit = {
    setNbTests(100)
    setNVar(5)
    setRangeForAll((-10, 10))
    setDensityForAll(0.2)
    randomSeed()
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