import checker.NoSolutionException
import checker.constraints.Element
import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.ConstraintsName
import org.chocosolver.solver.constraints.nary.element.PropElementV_fast
import org.chocosolver.solver.variables.IntVar

object ChocoElementTest {
  var size = 0

  def main(args: Array[String]) {

    //set the x
    val c = new Element
    c.gen.setNVar(7)
    c.gen.addVar(1.5 / 7.0, (0, 10))
    c.gen.addVar(0.1, (-10, 10))
    c.checkAC(
      variables => {
        size = variables.length
        if (variables.length < 3) throw NoSolutionException()
        val model: Model = new Model("Element constraint")
        val x: Array[IntVar] = new Array[IntVar](variables.length)
        for (i <- variables.indices) {
          val b: Array[Int] = variables(i).toArray
          x(i) = model.intVar("x" + i, b)
        }
        val i: IntVar = x(x.length - 2)
        val v: IntVar = x(x.length - 1)
        val y: Array[IntVar] = x.dropRight(2)
        val cstr = new org.chocosolver.solver.constraints.Constraint(ConstraintsName.ELEMENT,
          new PropElementV_fast(v, y, i, 0, false))
        model.post(cstr)
        //this commented code do a fast element constraint => it does not enforce AC
        //model.element(v,y,i,0).post()
        try {
          model.getSolver.propagate()
        } catch {
          case _: Exception => throw NoSolutionException();
        }
        val result: Array[Set[Int]] = x.map {
          vari =>
            var s: Set[Int] = Set(vari.getUB)
            var elem = vari.getLB
            while (elem != vari.getUB) {
              s = s + elem
              elem = vari.nextValue(elem)
            }
            s
        }
        result
      },
      solution => {
        if (size < 3) false
        else if (solution.length == size) {
          val i = solution(size - 2)
          val v = solution(size - 1)
          if (i < 0 || i >= size - 2) false
          else solution(i) == v
        } else true
      }
    )
  }
}
