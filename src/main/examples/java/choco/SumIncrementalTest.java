package choco;


import checker.*;
import checker.filterings.BoundZFiltering;
import checker.incremental.*;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;

/*
 * Incrementally testing choco's filtering for the constraint sum(x) == 15.
 * We will test that it reaches bound(Z) consistency and we will also test
 * the state restoration using trailing (push/pop).
 */
public class SumIncrementalTest {

    public static void main(String[] args) {
        // choco's filtering for the constraint sum(x)==15
        class testedFilter extends JFilterWithState {
            private Model model;
            private IntVar[] x;

            public Set<Integer>[] setupJava(Set<Integer>[] variables) {
                model = new Model("sum problem");
                x = new IntVar[variables.length];
                for (int i = 0; i < variables.length; i++) {
                    int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                    x[i] = model.intVar("" + i, b);
                }
                model.sum(x, "=", 15).post();
                Solver solver = model.getSolver();
                try {
                    solver.propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
                return transform(x);
            }


            public Set<Integer>[] branchAndFilterJava(BranchOp b) {
                if (b instanceof Push) {
                    model.getEnvironment().worldPush();
                } else if (b instanceof Pop) {
                    model.getEnvironment().worldPop();
                    Constraint[] cs = model.getCstrs();
                    model.unpost(cs[cs.length - 1]);
                } else if (b instanceof RestrictDomain) {
                    RestrictDomain r = (RestrictDomain) b;
                    model.arithm(x[r.index()], r.op(), r.constant()).post();
                    try {
                        model.getSolver().propagate();
                    } catch (Exception e) {
                        throw new NoSolutionException("");
                    }
                }
                return transform(x);
            }
        }

        // the trusted filtering with which the choco's filtering will be compared
        IncrementalFiltering trusted = new IncrementalFiltering(new BoundZFiltering(Checkers.sum(15, "=")));

        // checking that both algorithms return the same filtered domains over random test instances
        CPChecker.check(trusted, new testedFilter(),
                CPChecker.testArguments(), CPChecker.stats());
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
