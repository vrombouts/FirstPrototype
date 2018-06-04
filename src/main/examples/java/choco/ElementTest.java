package choco;

import checker.*;
import checker.Statistics;
import checker.filterings.ArcFiltering;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.ConstraintsName;
import org.chocosolver.solver.constraints.binary.element.PropElement;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

/*
 * Testing the choco's filtering algorithm for the constraint x[i] == v for a fixed x.
 */
public class ElementTest {

    // The array x considered for the element constraint x[i] == v with a fixed array x
    private static int[] x = {5, 1, 2, 4, 7, 9};

    public static void main(String[] args) {

        // choco's filtering to be tested for the constraint x[i]==v
        class testedFilter extends JFilter {
            public Set<Integer>[] filterJava(Set<Integer>[] variables) {
                Model model = new Model("Testing choco's element filtering");
                int[] b = variables[0].stream().mapToInt(Number::intValue).toArray();
                IntVar in = model.intVar(b);
                b = variables[1].stream().mapToInt(Number::intValue).toArray();
                IntVar v = model.intVar(b);
                org.chocosolver.solver.constraints.Constraint cstr = new org.chocosolver.solver.constraints.Constraint(ConstraintsName.ELEMENT,
                        new PropElement(v, x, in, 0));
                model.post(cstr);
                Solver solver = model.getSolver();
                try {
                    solver.propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
                IntVar[] finalVars = {in, v};
                return transform(finalVars);
            }
        }
        // sets the test parameters
        TestArgs parameters = new TestArgs();
        parameters.setNVar(2);
        parameters.setDensity(0, 0.5);
        parameters.setRange(0, 0, 6);
        parameters.setDensity(1, 0.5);
        parameters.setRange(1, 0, 10);
        Statistics stats = new Statistics("");

        // the trusted algorithm to be compared with the choco ones
        ArcFiltering trusted = new ArcFiltering(elementChecker());

        // checking the equality of both algorithms over some random domains
        CPChecker.check(trusted, new testedFilter(), parameters, stats);
    }

    private static Function<Integer[], Boolean> elementChecker() {
        return solution -> {
            int i = solution[0];
            int v = solution[1];
            if (i < 0 || i >= x.length) return false;
            else return x[i] == v;
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
