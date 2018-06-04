package choco;

import checker.*;
import checker.Statistics;
import checker.filterings.BoundZFiltering;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

/*
 * Testing statically that choco's filtering of the constraint sum(x)==5
 * reaches bound(Z) consistency.
 */
public class SumCheckBCTest {

    public static void main(String[] args) {

        /*
         * choco's filtering for the sum constraint to be tested
         */
        class testedFilter extends JFilter {
            public Set<Integer>[] filterJava(Set<Integer>[] variables) {

                Model model = new Model("Testing choco's sum filtering");
                IntVar[] x = new IntVar[variables.length];
                for (int i = 0; i < variables.length; i++) {
                    int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                    x[i] = model.intVar("" + i, b);
                }
                model.sum(x, "=", 5).post();
                Solver solver = model.getSolver();
                try {
                    solver.propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
                return transform(x);
            }
        }

        // setting the test parameters
        TestArgs parameters = new TestArgs();
        Statistics stats = new Statistics("");

        // the trusted filtering with which choco's filtering will be tested
        Filter trusted = new BoundZFiltering(sumChecker());

        // checking that both algorithms filters in the same way over some random test instances
        CPChecker.check(trusted, new testedFilter(), parameters, stats);
    }

    private static Function<Integer[], Boolean> sumChecker() {
        return integers -> {
            int sum = 0;
            for (int i = 0; i < integers.length; i++) {
                sum += integers[i];
            }
            return sum == 5;
        };
    }

    /*
     * returns the domains in the type Set<Integer> from the choco
     * domains' type IntVar
     */
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
