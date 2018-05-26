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

public class ElementTest {

    private static int[] x = {5, 1, 2, 4, 7, 9};

    public static void main(String[] args) {

        class MyFilter extends JFilter {
            public Set<Integer>[] filterJava(Set<Integer>[] variables) {
                Model model = new Model("Testing choco's element filtering");
                int[] b = variables[0].stream().mapToInt(Number::intValue).toArray();
                IntVar in = model.intVar(b);
                b = variables[1].stream().mapToInt(Number::intValue).toArray();
                IntVar v = model.intVar(b);
                org.chocosolver.solver.constraints.Constraint cstr = new org.chocosolver.solver.constraints.Constraint(ConstraintsName.ELEMENT,
                        new PropElement(v, x, in, 0));
                model.post(cstr);
                //model.element(v, x, in, 0).post();

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
        TestArgs parameters = new TestArgs();
        parameters.setNVar(2);
        parameters.setDensity(0, 0.5);
        parameters.setRange(0, 0, 6);
        parameters.setDensity(1, 0.5);
        parameters.setRange(1, 0, 10);
        ArcFiltering elementary = new ArcFiltering(elementChecker());
        Statistics stats = new Statistics("");
        CPChecker.check(elementary, new MyFilter(), parameters, stats);
    }

    private static Function<Integer[], Boolean> elementChecker() {
        return solution -> {
            int i = solution[0];
            int v = solution[1];
            if (i < 0 || i >= x.length) return false;
            else return x[i] == v;
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
