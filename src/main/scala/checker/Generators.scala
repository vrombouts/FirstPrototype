package checker
import Conversions._

object Generators {

  def table(t:Set[Array[Int]]):VariablesGenerator = {
    val min: Int = t.map(x => x.min).min
    val max: Int = t.map(x => x.max).max
    val gen:VariablesGenerator = new VariablesGenerator
    gen.setNVar(t.last.length)
    gen.setRangeForAll(min, max)
    gen.setDensityForAll(3 * 1 / (max - min))
    gen
  }

  def table(t:java.util.Set[Array[Integer]]):VariablesGenerator = {
    table(tableToScala(t))
  }

  def gcc(values:Array[Int]):VariablesGenerator = {
    val gen:VariablesGenerator = new VariablesGenerator
    gen.baseRange = (values.min, values.max)
    gen.baseDensity = 3 / values.length
    gen.setNVar(8)
    gen.baseRange = (0, 8)
    gen.baseDensity = 2.0 / 8.0
    gen.addNVar(values.length)
    gen
  }

  def sum(operator:String, constant:Int):VariablesGenerator = {
    val gen:VariablesGenerator = new VariablesGenerator
    gen.reset()
    gen.setNVar(10)
    val middleValue = constant/10
    if(operator.equals("=")) {
      gen.setRangeForAll(middleValue-1, middleValue+1)
      gen.setDensityForAll(0.7)
    }
    else if(operator.equals("!=")) {
      gen.setRangeForAll(middleValue-1, middleValue+1)
      gen.setDensityForAll(0.1)
      gen.setDensity(0,0.5)
    }
    else if(operator.equals(">")) {
      gen.setRangeForAll(middleValue-5, middleValue+3)
      gen.setDensityForAll(0.3)
    }
    else if(operator.equals("<")){
      gen.setRangeForAll(middleValue-2, middleValue+5)
      gen.setDensityForAll(0.3)
    }
    else if(operator.equals(">=")){
      gen.setRangeForAll(middleValue-5, middleValue+3)
      gen.setDensityForAll(0.3)
    }
    else if(operator.equals("<=")) {
      gen.setRangeForAll(middleValue-2, middleValue+5)
      gen.setDensityForAll(0.3)
    }
    gen
  }

  def element:VariablesGenerator = {
    val gen: VariablesGenerator = new VariablesGenerator
    gen.reset()
    gen.setNVar(10)
    gen.addVar(0.2, (-1, 12))
    gen.addVar(0.1, (-11, 11))
    gen
  }

  def allDifferent:VariablesGenerator = {
    val gen:VariablesGenerator  =new VariablesGenerator
    gen.setRangeForAll((-5, 5))
    gen
  }

}
