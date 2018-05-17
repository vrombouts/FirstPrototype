package checker

import Conversions._

object Generators {

  def table(t: Set[Array[Int]]): TestArgs = {
    val min: Int = t.map(x => x.min).min
    val max: Int = t.map(x => x.max).max
    val gen: TestArgs = new TestArgs
    gen.setNVar(t.last.length)
    gen.setRangeForAll(min, max)
    gen.setDensityForAll(3 * 1 / (max - min))
    gen
  }

  def table(t: java.util.Set[Array[Integer]]): TestArgs = {
    table(tableToScala(t))
  }

  def gcc(values: Array[Int]): TestArgs = {
    val gen: TestArgs = new TestArgs
    gen.baseRange = (values.min, values.max)
    gen.baseDensity = 3 / values.length
    gen.setNVar(8)
    gen.baseRange = (0, 8)
    gen.baseDensity = 2.0 / 8.0
    gen.addNVar(values.length)
    gen
  }

  def sum(operator: String, constant: Int): TestArgs = {
    val gen: TestArgs = new TestArgs
    gen.reset()
    gen.setNVar(10)
    val middleValue = constant / 10
    if (operator.equals("=")) {
      gen.setRangeForAll(middleValue - 1, middleValue + 1)
      gen.setDensityForAll(0.7)
    }
    else if (operator.equals("!=")) {
      gen.setRangeForAll(middleValue - 1, middleValue + 1)
      gen.setDensityForAll(0.1)
      gen.setDensity(0, 0.5)
    }
    else if (operator.equals(">")) {
      gen.setRangeForAll(middleValue - 5, middleValue + 3)
      gen.setDensityForAll(0.3)
    }
    else if (operator.equals("<")) {
      gen.setRangeForAll(middleValue - 2, middleValue + 5)
      gen.setDensityForAll(0.3)
    }
    else if (operator.equals(">=")) {
      gen.setRangeForAll(middleValue - 5, middleValue + 3)
      gen.setDensityForAll(0.3)
    }
    else if (operator.equals("<=")) {
      gen.setRangeForAll(middleValue - 2, middleValue + 5)
      gen.setDensityForAll(0.3)
    }
    gen
  }

  def element: TestArgs = {
    val gen: TestArgs = new TestArgs
    gen.reset()
    gen.setNVar(8)
    gen.addVar(0.2, (-1, 9))
    gen.addVar(0.1, (-11, 10))
    gen
  }

  def allDifferent: TestArgs = {
    val gen: TestArgs = new TestArgs
    gen.setRangeForAll((-5, 5))
    gen
  }

}
