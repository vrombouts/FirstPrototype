package jacop;

import checker.*;
import org.jacop.core.*;
import org.jacop.constraints.*;
import org.jacop.search.*;

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
        CPChecker.check(trusted,tested,
                CPChecker.testArguments(),CPChecker.stats());
    }


    static public Set<Integer>[] filteringAllDifferentBC(Set<Integer>[] variables){
        Store store = new Store();
        IntVar[] jacopVars = new IntVar[variables.length];
        for(int i=0; i<variables.length;i++){
            jacopVars[i] = toIntVar(variables[i], store);
        }
        Alldiff constraint = new Alldiff(jacopVars);
        store.impose(constraint);
        try {
            constraint.consistency(store);
            if (!store.consistency()) throw new NoSolutionException("no solutions");
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

    static public IntVar toIntVar(Set<Integer> ss,Store store){
        IntVar var = new IntVar(store);
        for(Integer s : ss){
            var.addDom(s,s);
        }
        return var;
    }

}
