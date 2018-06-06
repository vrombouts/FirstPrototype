package checker

import org.scalacheck.Test.{Parameters, TestCallback}
import org.scalacheck.{Gen, Test}

import scala.util.Random

/**
  * This class represents the different arguments that the
  * user can set for his tests.
  */
class TestArgs {

  //number of dives performed with incremental filterings
  var nbDive = 10
  //by default, generated random domain have 5 variables with range [-10,10]
  // and with 0.2 density.
  private[this] var nbVars: Int = 5
  private[this] var densities: Array[Double] = Array.fill(nbVars)(4.0 / 20.0)
  private[this] var ranges: Array[(Int, Int)] = Array.fill(nbVars)((-10, 10))
  var baseRange: (Int, Int) = (-10, 10)
  var baseDensity: Double = 4.0 / 20.0
  val random: Random = new Random()
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

  /**
    * This function set the seed used for a test.
    *
    * @param sd : a seed
    */
  def setSeed(sd: Long): Unit = {
    seed = Some(sd)
    random.setSeed(sd)
  }

  /**
    * This function returns the seed used for the tests.
    * If no seed is set, a random number is returned.
    */
  def getSeed: Long = seed match {
    case Some(sd) => sd
    case None => random.nextLong
  }

  /**
    * If a seed is set, this one is unset.
    */
  def randomSeed(): Unit = {
    seed = None
    random.setSeed(new Random().nextLong)
  }

  /**
    * @return the number of variables generated per test instance
    */
  def getNbVars: Int = nbVars

  /**
    * set the number of tests to 'n'.
    */
  def setNbTests(n: Int): Unit = nbTests = Some(n)

  /**
    * @return the number of tests perfomed. 100 by default
    */
  def getNbTests: Int = nbTests match {
    case Some(n) => n
    case _ => 100
  }

  def getTestParameters: Parameters = {
    (seed, nbTests) match {
      case (Some(l), Some(t)) => new testParams(l, t)
      case (Some(l), None) => new testParams(l)
      case (None, Some(t)) => new testParams(t)
      case _ => new testParams
    }
  }

  /**
    * @return the Scalacheck generator used for generating
    *         the tests instances.
    */
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

  /**
    * @param n : an integer
    *          if 'n' is not a non-zero positive integer, nothing happen.
    *          otherwise, the tests instances are set to be with N variables
    *          with the basic range and density.
    */
  def setNVar(n: Int): Unit = {
    nbVars = if (n < 1) 1 else n
    densities = Array.fill(nbVars)(baseDensity)
    ranges = Array.fill(nbVars)(baseRange)
  }

  /**
    * add a variable with the basic range and density
    * at the end
    */
  def addVar(): Unit = {
    nbVars += 1
    densities = densities :+ baseDensity
    ranges = ranges :+ baseRange
  }

  /**
    * add a variable with a given 'range' and 'density'
    * at the end
    */
  def addVar(density: Double, range: (Int, Int)): Unit = {
    nbVars += 1
    densities = densities :+ density
    ranges = ranges :+ range
  }

  /**
    * add a variable with a given range ['min','max'] and 'density'
    * at the end
    */
  def addVar(density: Double, min: Int, max: Int): Unit = {
    nbVars += 1
    densities = densities :+ density
    ranges = ranges :+ (min, max)
  }

  /**
    * This function add n variables to the instances generated
    * with the basic density and range at the end
    */
  def addNVar(n: Int): Unit = {
    for (_ <- 1 to n) addVar()
  }

  /**
    * This function add n variables to the instances generated
    * with the given density and range at the end
    */
  def addNVar(n: Int, density: Double, range: (Int, Int)): Unit = {
    for (_ <- 1 to n) addVar(density, range)
  }

  /**
    * This function add n variables to the instances generated
    * with the given density and range
    */
  def addNVar(n: Int, density: Double, min: Int, max: Int): Unit = {
    for (_ <- 1 to n) addVar(density, (min, max))
  }


  /**
    * set the density of the 'index'th variable to 'density'.
    */
  def setDensity(index: Int, density: Double): Unit = {
    var d = densities(index)
    if (density <= 1.0 && density >= 0.0)
      d = density
    densities(index) = d
  }

  /**
    * set the density of all the variables to 'density'
    * while setting it as the default density
    */
  def setDensityForAll(density: Double): Unit = {
    if (density <= 1.0 && density >= 0.0)
      baseDensity = density
    densities = Array.fill(nbVars)(baseDensity)
  }

  /**
    * set the range of the 'index'th variable to 'range'.
    */
  def setRange(index: Int, range: (Int, Int)): Unit = ranges(index) = range

  /**
    * set the range of the 'index'th variable to ['min','max'].
    */
  def setRange(index: Int, min: Int, max: Int): Unit = ranges(index) = (min, max)

  /**
    * set the range of all the variables to ['min','max']
    * while setting it as the default range
    */
  def setRangeForAll(min: Int, max: Int): Unit = {
    baseRange = (min, max)
    ranges = Array.fill(nbVars)(baseRange)
  }

  /**
    * set the range of all the variables to 'range'
    * while setting it as the default range
    */
  def setRangeForAll(range: (Int, Int)): Unit = {
    baseRange = range
    ranges = Array.fill(nbVars)(baseRange)
  }

  /**
    * remove the 'index'th variable
    */
  def remove(index: Int): Unit = {
    if (index < 0 || index >= nbVars) return
    nbVars -= 1
    var j = -1
    densities = densities.filterNot { _ => j += 1; j == index }
    j = -1
    ranges = ranges.filterNot { _ => j += 1; j == index }
  }

  /**
    * reset the current test arguments to the default values
    */
  def reset(): Unit = {
    setNbTests(100)
    setNVar(5)
    setRangeForAll((-10, 10))
    setDensityForAll(0.2)
    nbDive = 10
    randomSeed()
  }

  /**
    * @return a String representing the generator of variables
    */
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