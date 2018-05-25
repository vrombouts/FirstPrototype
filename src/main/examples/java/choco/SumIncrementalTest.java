package choco;


import checker.*;
import checker.incremental.*;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;

public class SumIncrementalTest {

    public static void main(String[] args) {
        class MyFilter extends JFilterWithState {
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
        IncrementalFiltering trusted = new IncrementalFiltering(new BoundZFiltering(Checkers.sum(15, "=")));
        CPChecker.check(trusted, new MyFilter(),
                CPChecker.testArguments(), CPChecker.stats());
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
