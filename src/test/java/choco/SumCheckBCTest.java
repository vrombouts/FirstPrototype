package choco;

import checker.JCpChecker;
import checker.NoSolutionException;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;


public class SumCheckBCTest {
    private static IntVar[] x;

    public static void main(String[] args) {
        JCpChecker jc = new JCpChecker();
        jc.checkBC(variables -> {
                    Model model = new Model("sum problem");
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
                },
                integers -> {
                    if (integers.length != x.length) return true;
                    int sum = 0;
                    for (int i = 0; i < integers.length; i++) {
                        sum += integers[i];
                    }
                    return sum == 5;
                });
    }


    public static Set<Integer>[] transform(IntVar[] input) {
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
