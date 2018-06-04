package choco;

import checker.*;
import checker.Statistics;
import checker.prunings.ArcPruning;
import checker.prunings.BoundZPruning;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.nary.alldifferent.PropAllDiffAC;
import org.chocosolver.solver.constraints.nary.alldifferent.PropAllDiffBC;
import org.chocosolver.solver.variables.IntVar;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

/*
 * Testing Choco's arc consistent and bound(Z) consistent filtering algorithms for
 * the allDifferent constraint. Here we will test that they reach the correct consistency.
 */
public class AllDifferentTest {

    private static void testAllDifferentBC() {
        class testedFilter extends JFilter {
            public Set<Integer>[] filterJava(Set<Integer>[] domains) {
                return filteringBC(domains);
            }
        }
        // changing the test parameters
        TestArgs parameters = new TestArgs();
        parameters.setRangeForAll(-5, 5);
        parameters.setSeed(150);
        Statistics stats = new Statistics("");

        // the trusted filtering. Since allDifferent is an anti-monotonic function,
        // the BoundZPruning class is considered here. We could consider the
        // BoundZFiltering instead.
        BoundZPruning trustedFilter = new BoundZPruning(checkerAllDifferent());

        // checking the tested filtering and the trusted one return the same domains over the test instances
        CPChecker.check(trustedFilter, new testedFilter(), parameters, stats);
    }

    private static void testAllDifferentAC() {
        class MyFilter extends JFilter {
            public Set<Integer>[] filterJava(Set<Integer>[] domains) {
                return filteringAC(domains);
            }
        }
        TestArgs generator = new TestArgs();
        generator.setRangeForAll(-5, 5);
        generator.setSeed(150);
        Statistics stats = new Statistics("");
        CPChecker.check(new ArcPruning(Checkers.allDifferent()), new MyFilter(), generator, stats);
    }

    /*
     * The choco's AC filtering of the allDifferent constraint
     */
    private static Set<Integer>[] filteringAC(Set<Integer>[] variables) {
        Model model = new Model("testing choco's filtering for allDifferent");
        IntVar[] x = new IntVar[variables.length];
        for (int i = 0; i < variables.length; i++) {
            int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
            x[i] = model.intVar("" + i, b);
        }
        PropAllDiffAC cstr = new PropAllDiffAC(x);
        try {
            cstr.propagate(0);
        } catch (Exception e) {
            throw new NoSolutionException("No solution");
        }
        return transform(x);
    }

    /*
     * The choco's BC filtering of the allDifferent constraint
     */
    private static Set<Integer>[] filteringBC(Set<Integer>[] variables) {
        Model model = new Model("allDifferent problem");
        IntVar[] x = new IntVar[variables.length];
        for (int i = 0; i < variables.length; i++) {
            int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
            x[i] = model.intVar("" + i, b);
        }
        PropAllDiffBC cstr = new PropAllDiffBC(x);
        try {
            cstr.propagate(0);
        } catch (Exception e) {
            throw new NoSolutionException("No solution");
        }
        return transform(x);
    }


    private static Function<Integer[], Boolean> checkerAllDifferent() {
        return integers -> {
            Set s = new HashSet<Integer>(Arrays.asList(integers));
            return s.size() == integers.length;
        };
    }

    public static void main(String[] args) {
        System.out.println("Begin test allDifferentBC");
        testAllDifferentBC();
        System.out.println("End test allDifferentBC");

        System.out.println("Begin test allDifferentAC");
        testAllDifferentAC();
        System.out.println("End test allDifferentAC");
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