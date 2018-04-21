package choco;

import checker.JCpChecker;
import checker.NoSolutionException;
import checker.constraints.Sum;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Operator;
import org.chocosolver.solver.constraints.nary.alldifferent.PropAllDiffBC;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

/*
 * Tests for the sum constraint implementation of the choco solver
 */
public class SumTest {


    public static void main(String[] args) {
        Sum sum = new Sum("<", 20);
        JCpChecker jc = new JCpChecker(sum);
        jc.checkBC(variables -> {
            Model model = new Model("sum problem");
            IntVar[] x = new IntVar[variables.length];
            for (int i = 0; i < variables.length; i++) {
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                x[i] = model.intVar("" + i, b);
            }
            if (x == null) System.out.println("x null!");
            model.sum(x, "<", 20).post();
            Solver solver = model.getSolver();
            try {
                solver.propagate();
            } catch (Exception e) {
                throw new NoSolutionException("");
            }
            return transform(x);
        }, null);
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
