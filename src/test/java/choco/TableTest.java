package choco;

import checker.JCpChecker;
import checker.NoSolutionException;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.extension.Tuples;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;

public class TableTest {

    public static void main(String[] args) {
        checkTable();
    }

    private static void checkTable() {
        JCpChecker jc = new JCpChecker();
        jc.checkTableAC((variables, tablee) -> {
            Model model = new Model("my first problem");
            // 2. Create variables
            IntVar[] x = new IntVar[variables.length];
            for (int i = 0; i < variables.length; i++) {
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                x[i] = model.intVar("" + i, b);
            }
            int[][] ii = new int[tablee.size()][variables.length];
            int i = 0;
            for (Integer[] s : tablee) {
                ii[i] = new int[s.length];
                for (int j = 0; j < s.length; j++) {
                    ii[i][j] = s[j];
                }
                i++;
            }
            Tuples tuples = new Tuples(ii, true);
            model.table(x, tuples).post();
            Solver solv = model.getSolver();
            try {
                solv.propagate();
            } catch (Exception e) {
                throw new NoSolutionException("");
            }
            return transform(x);
        });
    }

    private static Set<Integer>[] transform(IntVar[] input) {
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