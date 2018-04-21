package choco;

import checker.JCpChecker;
import checker.NoSolutionException;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.ConstraintsName;
import org.chocosolver.solver.constraints.nary.circuit.CircuitConf;
import org.chocosolver.solver.constraints.nary.circuit.PropCircuitSCC;
import org.chocosolver.solver.variables.IntVar;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

public class CircuitTest {
    private static IntVar[] currentVars;
    public static void main(String [] args){
        JCpChecker jc = new JCpChecker();
        jc.gen().setRangeForAll(0,4);
        jc.gen().setDensityForAll(0.8);
        jc.check(filter(),isSolution());
    }

    private static Function<Set<Integer>[], Set<Integer>[]> filter(){
        return variables ->{
            Model model = new Model("Testing circuitSCC implementation");
            currentVars = new IntVar[variables.length];
            for(int i=0; i<variables.length;i++) {
                int[] b = variables[i].stream().mapToInt(Number::intValue).toArray();
                currentVars[i] = model.intVar("" + i, b);
            }
            org.chocosolver.solver.constraints.Constraint ctr = new org.chocosolver.solver.constraints.Constraint(ConstraintsName.CIRCUIT,new PropCircuitSCC(currentVars, 0, CircuitConf.ALL));
            model.post(ctr);
            try{model.getSolver().propagate();}
            catch (Exception e){throw new NoSolutionException("No solution");}
            return transform(currentVars);
        };
    }

    private static boolean isSol(Integer [] variables, int index, int acc, boolean [] isVisited){
        if(variables[index]<0 || variables[index]>=variables.length)return false;
        if(isVisited[variables[index]]) return false;
        isVisited[variables[index]] = true;
        if(acc == variables.length-1){
            if(variables[index]==0) return true;
            else return false;
        }
        return isSol(variables,variables[index],acc+1,isVisited);
    }
    private static Function<Integer[], Boolean> isSolution(){
        return variables -> {
            if(variables.length != currentVars.length) return true;
            boolean[] isVisited = new boolean[variables.length];
            return isSol(variables,0,0,isVisited);
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
