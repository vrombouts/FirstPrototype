package choco;


import checker.*;
import checker.constraints.Sum;
import org.chocosolver.memory.IEnvironment;
import org.chocosolver.memory.trailing.EnvironmentTrailing;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.unary.PropEqualXC;
import org.chocosolver.solver.variables.IntVar;
import scala.collection.generic.BitOperations;

import java.lang.reflect.Array;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.function.Function;

public class TestsSumIncremental {
    private static IntVar [] x;
    private static Model model = new Model("sum problem");

    public static Function<Set<Integer> [],Set<Integer>[]> f() {
        return new Function<Set<Integer>[], Set<Integer>[]>() {
            @Override
            public Set<Integer>[] apply(Set<Integer>[] variables) throws NoSolutionException {
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
        };
    }

    public static void p(Set<Integer> [] input){
        for(int i=0; i<input.length;i++){
            System.out.print(input[i]);
        }
        System.out.println("");
    }

    public static void main(String[] args) {
        JCpChecker jc = new JCpChecker();
        jc.checkBC(f(),new Function<BranchOp,Set<Integer>[]>(){
            @Override
            public Set<Integer> [] apply(BranchOp b) throws NoSolutionException {
                IEnvironment env=model.getEnvironment();
                if(b instanceof Push) {
                    env.worldPush();
                    Solver s = model.getSolver();
                    try{s.propagate();} catch(Exception e){throw new NoSolutionException("No solution");}
                    p(transform(x));
                    return transform(x);
                }
                else if(b instanceof Pop)  {
                    env.worldPop();
                    Solver s = model.getSolver();
                    try{s.propagate();} catch(Exception e){throw new NoSolutionException("No solution");}
                    p(transform(x));
                    return transform(x);
                }
                else if(b instanceof RestrictDomain){
                    try{Set<Integer> [] v= remove((RestrictDomain)b); p(v);} catch(NoSolutionException e){System.out.println("hkjhkjg");}
                }
                return transform(x);
            }
        }, new Function<Integer[], Boolean>() {
            @Override
            public Boolean apply(Integer[] integers) {
                if(integers.length != x.length) return true;
                int sum=0;
                for(int i =0; i<integers.length;i++){
                    sum += integers[i];
                }
                return sum == 5;
            }
        });
    }

    public static Set<Integer>[] remove(RestrictDomain r) {
        switch (r.op) {
            case 0: {
                model.arithm(x[r.index], "=", r.constant).post();
                try {
                    model.getSolver().propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
            }
            case 1: {
                model.arithm(x[r.index], "!=", r.constant).post();
                try {
                    model.getSolver().propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
            }
            case 2: {
                model.arithm(x[r.index], "<", r.constant).post();
                try {
                    model.getSolver().propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
            }
            case 3: {
                model.arithm(x[r.index], "<=", r.constant).post();
                try {
                    model.getSolver().propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
            }
            case 4: {
                model.arithm(x[r.index], ">", r.constant).post();
                try {
                    model.getSolver().propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
            }
            case 5: {
                model.arithm(x[r.index], ">=", r.constant).post();
                try {
                    model.getSolver().propagate();
                } catch (Exception e) {
                    throw new NoSolutionException("");
                }
            }
        }
        return transform(x);
    }

    public static Set<Integer>[] transform(IntVar[] input){
        Set<Integer> [] result = new Set[input.length];
        for(int i = 0; i<input.length;i++){
            result[i] = new HashSet<Integer>();
        }
        for(int i=0; i<input.length;i++){
            int elem = input[i].getLB();
            int ub = input[i].getUB();
            while(elem != ub){
                result[i].add(elem);
                elem = input[i].nextValue(elem);
            }
            result[i].add(ub);
        }
        return result;
    }
}
