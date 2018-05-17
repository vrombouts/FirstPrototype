package jacop;

import checker.*;
import org.jacop.core.*;
import org.jacop.constraints.*;

import java.util.HashSet;
import java.util.Set;

public class AllDifferentBCTest {

    static public void main(String[] args){
        class MyFilter extends JFilter{
            @Override
            public Set<Integer>[] filterJava(Set<Integer>[] variables) {
                return filteringAllDifferentBC(variables);
            }
        }
        Filter trusted = new BCFiltering(Checkers.allDifferent());
        Filter tested  = new MyFilter();
        CPChecker.testArguments().setRangeForAll(-5,5);
        CPChecker.testArguments().setNbTests(1000);
        CPChecker.testArguments().setSeed(100);
        CPChecker.check(trusted,tested,
                CPChecker.testArguments(),CPChecker.stats());
    }


    static public Set<Integer>[] filteringAllDifferentBC(Set<Integer>[] variables){
        Store store = new Store();
        IntVar[] jacopVars = toIntVar(variables, store);
        Alldiff constraint = new Alldiff(jacopVars);
        store.impose(constraint);
        try {
            constraint.consistency(store);
            if (!store.consistency()) throw new NoSolutionException("no solutions");
            //store.imposeWithConsistency(constraint);
        }catch(FailException e){
            throw new NoSolutionException("fail exception");
        }
        return toSets(jacopVars);
    }

    static public Set<Integer>[] toSets(IntVar[] jacopVars){
        Set<Integer>[] variables = new Set[jacopVars.length];
        for(int i=0; i< jacopVars.length; i++){
            variables[i] = new HashSet<Integer>();
            int[] v = jacopVars[i].domain.toIntArray();
            for(int j=0; j<v.length;j++){
                variables[i].add(v[j]);
            }
        }
        return variables;
    }

    static public IntVar[] toIntVar(Set<Integer>[] vars,Store store){
        IntVar[] jacopVars = new IntVar[vars.length];
        for(int i=0; i<vars.length;i++) {
            jacopVars[i] = new IntVar(store);
            for (Integer value : vars[i]) {
                jacopVars[i].addDom(value, value);
            }
        }
        return jacopVars;
    }

}
