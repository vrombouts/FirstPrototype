package choco;

import checker.*;
import checker.statistics.StrongerStatistics;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ConstraintsName;
import org.chocosolver.solver.constraints.nary.circuit.CircuitConf;
import org.chocosolver.solver.constraints.nary.circuit.PropCircuitSCC;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

public class CircuitTest {
    private static IntVar[] currentVars;

    public static void main(String[] args) {
        class MyFilter extends JFilter {
            public Set<Integer>[] filterJava(Set<Integer>[] variables) {
                return circuitFiltering().apply(variables);
            }
        }
        TestArgs parameters = new TestArgs();
        parameters.setRangeForAll(0, 4);
        parameters.setDensityForAll(0.8);
        Filter bugfree = new ACFiltering(circuitChecker());
        Filter tested = new MyFilter();
        StrongerStatistics stats = new StrongerStatistics("");
        CPChecker.stronger(bugfree, tested, parameters, stats);
    }

    private static Function<Set<Integer>[], Set<Integer>[]> circuitFiltering() {
        return variables -> {
            Model model = new Model("Testing choco's circuitSCC filtering");
            currentVars = new IntVar[variables.length];
            for (int i = 0; i < variables.length; i++) {
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                currentVars[i] = model.intVar("" + i, b);
            }
            Constraint ctr = new Constraint(ConstraintsName.CIRCUIT, new PropCircuitSCC(currentVars, 0, CircuitConf.ALL));
            model.post(ctr);
            try {
                model.getSolver().propagate();
            } catch (Exception e) {
                throw new NoSolutionException("No solution");
            }
            return transform(currentVars);
        };
    }

    private static boolean circChecker(Integer[] variables, int index, int acc, boolean[] isVisited) {
        if (variables[index] < 0 || variables[index] >= variables.length) return false;
        if (isVisited[variables[index]]) return false;
        isVisited[variables[index]] = true;
        if (acc == variables.length - 1) {
            return (variables[index] == 0);
        }
        return circChecker(variables, variables[index], acc + 1, isVisited);
    }

    private static Function<Integer[], Boolean> circuitChecker() {
        return variables -> {
            boolean[] isVisited = new boolean[variables.length];
            return circChecker(variables, 0, 0, isVisited);
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
