package choco;

import checker.JCpChecker;
import checker.NoSolutionException;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.nary.alldifferent.PropAllDiffAC;
import org.chocosolver.solver.constraints.nary.alldifferent.PropAllDiffBC;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

public class TestsAllDifferent {

    public static void testAllDifferentBC(JCpChecker jc){
        jc.checkBC(applyBC(), applyAllDifferent());
    }

    public static void testAllDifferentAC(JCpChecker jc){
        jc.checkAC(applyAC(), applyAllDifferent());
    }

    public static void testAllDifferentBC2(JCpChecker jc){
        jc.checkAllDifferentBC(applyBC());
    }

    public static void testAllDifferentAC2(JCpChecker jc){
        jc.checkAllDifferentAC(applyAC());
    }

    public static Function<Set<Integer>[],Set<Integer>[]> applyAC(){
        return variables -> {
            Model model = new Model("allDifferent problem");
            IntVar[] x = new IntVar[variables.length];
            for(int i=0;i<variables.length;i++){
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                x[i] = model.intVar(""+i,b);
            }
            PropAllDiffAC cstr = new PropAllDiffAC(x);
            try {
                cstr.propagate(0);
            } catch(Exception e){throw new NoSolutionException("No solution");}
            return transform(x);
        };
    }

    public static Function<Set<Integer>[],Set<Integer>[]> applyBC(){
        return variables -> {
            Model model = new Model("allDifferent problem");
            IntVar[] x = new IntVar[variables.length];
            for(int i=0;i<variables.length;i++){
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                x[i] = model.intVar(""+i,b);
            }
            PropAllDiffBC cstr = new PropAllDiffBC(x);
            try {
                cstr.propagate(0);
            } catch(Exception e){throw new NoSolutionException("No solution");}
            return transform(x);
        };
    }

    public static Function<Integer[],Boolean> applyAllDifferent() {
        return integers -> {
            for (int i = 0; i < integers.length; i++) {
                for (int j = 0; j < i; j++) {
                    if (integers[i].equals(integers[j]))
                        return false;
                }
            }
            return true;
        };
    }

    public static void main(String[] args) {
        JCpChecker jc = new JCpChecker();
        System.out.println("Begin test allDifferentBC");
        testAllDifferentBC(jc);
        System.out.println("End test allDifferentBC");

        System.out.println("Begin test allDifferentAC");
        testAllDifferentAC(jc);
        System.out.println("End test allDifferentAC");

        System.out.println("Begin test allDifferentBC2");
        testAllDifferentBC2(jc);
        System.out.println("End test allDifferentBC2");

        System.out.println("Begin test allDifferentAC2");
        testAllDifferentAC2(jc);
        System.out.println("End test allDifferentAC2");
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