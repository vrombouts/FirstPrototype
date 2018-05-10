package choco;


import checker.*;
import checker.incremental.BranchOp;
import checker.incremental.Pop;
import checker.incremental.Push;
import checker.incremental.RestrictDomain;
import checker.Statistics;
import org.chocosolver.memory.IEnvironment;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;
import java.util.function.Function;

public class SumIncrementalTest {

    public static void main(String[] args) {
        class MyFilter extends JFilterWithState {
            private IntVar[] x;
            private Model model = new Model("Testing choco's sum incremental");
            private Stack<Integer> state = new Stack<Integer>();
            private int nb = 0;

            public Set<Integer>[] setupJava(Set<Integer>[] variables) {
                model = new Model("sum problem");
                x = new IntVar[variables.length];
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


            public Set<Integer>[] branchAndFilterJava(BranchOp b) {
                IEnvironment env = model.getEnvironment();
                if (b instanceof Push) {
                    env.worldPush();
                    state.push(nb);
                    nb = 0;
                    Solver s = model.getSolver();
                    try {
                        s.propagate();
                    } catch (Exception e) {
                        throw new NoSolutionException("No solution");
                    }
                    return transform(x);
                } else if (b instanceof Pop) {
                    env.worldPop();
                    Constraint[] cs = model.getCstrs();
                    cs = Arrays.copyOfRange(cs, cs.length - nb, cs.length);
                    model.unpost(cs);
                    //s.restoreRootNode();
                    nb = state.pop();
                    Solver s = model.getSolver();
                    try {
                        s.propagate();
                    } catch (Exception e) {
                        throw new NoSolutionException("No solution");
                    }
                    return transform(x);
                } else if (b instanceof RestrictDomain) {
                    nb++;
                    try {
                        Set<Integer>[] v = remove((RestrictDomain) b);
                    } catch (NoSolutionException e) {
                        throw new NoSolutionException("No sol for restrict domain");
                    }
                }
                return transform(x);
            }

            private Set<Integer>[] remove(RestrictDomain r) {
                model.arithm(x[r.index()], r.op(), r.constant()).post();
                try {
                    model.getSolver().propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
                return transform(x);
            }
        }
        TestArgs parameters = new TestArgs();
        Statistics stats = new Statistics("");
        CPChecker.check(new BCFilteringIncremental(sumChecker()), new MyFilter(), parameters, stats);
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
