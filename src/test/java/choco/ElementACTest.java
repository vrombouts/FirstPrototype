package choco;

import checker.JCpChecker;
import checker.NoSolutionException;
import checker.constraints.BasicConstraint;
import checker.constraints.Constraint2;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.ConstraintsName;
import org.chocosolver.solver.constraints.nary.element.PropElementV_fast;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;

public class ElementACTest {

    public static void main(String[] args) {
        Constraint2 c = new BasicConstraint();
        c.gen().setNVar(7);
        c.gen().addVar(1.5 / 7.0, 0, 10);
        c.gen().addVar(0.1, -10, 10);
        JCpChecker jc = new JCpChecker(new BasicConstraint());
        jc.checkAC(variables -> {
            if (variables.length < 3)
                throw new NoSolutionException("Element must have at least three variables (x,i,v)");
            Model model = new Model("element constraint");
            IntVar[] x = new IntVar[variables.length - 2];
            for (int i = 0; i < variables.length - 2; i++) {
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                x[i] = model.intVar("" + i, b);
            }
            int[] b = variables[variables.length - 2].stream().mapToInt(Number::intValue).toArray();
            IntVar in = model.intVar(b);
            b = variables[variables.length - 1].stream().mapToInt(Number::intValue).toArray();
            IntVar v = model.intVar(b);
            if (x == null) System.out.println("x null!");
            //org.chocosolver.solver.constraints.Constraint cstr = new org.chocosolver.solver.constraints.Constraint(ConstraintsName.ELEMENT,
                 //   new PropElementV_fast(v, x, in, 0, false));
            //model.post(cstr);
            model.element(v,x,in,0).post();
            Solver solver = model.getSolver();
            try {
                solver.propagate();
            } catch (Exception e) {
                throw new NoSolutionException("");
            }
            IntVar[] finalVars = new IntVar[variables.length];
            for (int k = 0; k < variables.length - 2; k++) {
                finalVars[k] = x[k];
            }
            finalVars[variables.length - 2] = in;
            finalVars[variables.length - 1] = v;
            return transform(finalVars);
        }, solution -> {
            if (solution.length < 3) return false;
            int i = solution[solution.length - 2];
            int v = solution[solution.length - 1];
            if (i < 0 || i >= solution.length - 2) return false;
            else return solution[i] == v;
        });
    }


    public static Set<Integer>[] transform(IntVar[] input) {
        Set<Integer>[] result = new Set[input.length];
        for (int i = 0; i < input.length; i++) {
            result[i] = new HashSet<Integer>();
        }
        for (int i = 0; i < input.length; i++) {
            int elem = input[i].getLB();
            int ub = input[i].getUB();
            while (elem != ub) {
                result[i].add(elem);
                elem = input[i].nextValue(elem);
            }
            result[i].add(ub);
        }
        return result;
    }
}
