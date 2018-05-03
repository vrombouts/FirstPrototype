package choco;

import checker.*;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.extension.Tuples;
import org.chocosolver.solver.variables.IntVar;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

public class TableTest {
    private static Set<Integer[]> table = new HashSet();


    public static void main(String[] args) {
        Integer[] tab1 = {1, 2, 3};
        Integer[] tab2 = {2, 2, 3};
        Integer[] tab3 = {2, 3, 3};
        Integer[] tab4 = {1, 2, 4};
        table.add(tab1);
        table.add(tab2);
        table.add(tab3);
        table.add(tab4);

        class MyFilter extends JFilter {
            public Set<Integer>[] filterJava(Set<Integer>[] variables) {
                Model model = new Model("testing choco's table filtering");
                IntVar[] x = new IntVar[variables.length];
                for (int i = 0; i < variables.length; i++) {
                    int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                    x[i] = model.intVar("" + i, b);
                }
                int[][] ii = new int[table.size()][variables.length];
                int i = 0;
                for (Integer[] s : table) {
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
            }
        }
        VariablesGenerator gen = Generators.table(table);
        CPChecker.check(new ACFiltering(tableChecker()), new MyFilter(), gen);
    }

    private static Function<Integer[], Boolean> tableChecker() {
        return solution -> {
            for (Integer[] element : table) {
                if (Arrays.equals(element, solution))
                    return true;
            }
            return false;
        };
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