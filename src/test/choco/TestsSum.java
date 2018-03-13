package choco;

import checker.JCpChecker;
import checker.NoSolutionException;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Operator;
import org.chocosolver.solver.constraints.nary.alldifferent.PropAllDiffBC;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

/*
 * Tests for the sum constraint implementation of the choco solver
 */
public class TestsSum {



    public static void main(String[] args) {
        JCpChecker jc = new JCpChecker();
        jc.checkSumGT(variables -> {
            Model model = new Model("sum problem");
            IntVar[] x = new IntVar[variables.length-1];
            for(int i=0;i<variables.length-1;i++){
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                x[i] = model.intVar(""+i,b);
            }
            int index=variables.length-1;// the index of the sum variable
            int[] b = variables[index].stream().mapToInt(Number::intValue).toArray();
            IntVar s = model.intVar(""+index,b);
            if(x==null)System.out.println("x null!");
            if(s==null)System.out.println("s null!");
                model.sum(x, ">",s).post();
                Solver solver=model.getSolver();
            try{ solver.propagate();
            }catch (Exception e){throw new NoSolutionException("");}
            IntVar[] finals = new IntVar[variables.length];
            for(int i=0; i<finals.length;i++){
                if(i<x.length) finals[i]=x[i];
                else if(i==finals.length-1) finals[i]=s;
            }
            return transform(finals);
        });
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
