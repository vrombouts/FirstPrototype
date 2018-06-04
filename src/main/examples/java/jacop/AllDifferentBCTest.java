package jacop;

import checker.*;
import checker.filterings.BoundZFiltering;
import org.jacop.core.*;
import org.jacop.constraints.*;

import java.util.HashSet;
import java.util.Set;

/*
 * Testing the bound(Z) consistent jacop filtering for the allDifferent constraint.
 * We will test that the algorithm reached bound(Z) consistency.
 */
public class AllDifferentBCTest {

    static public void main(String[] args) {
        class MyFilter extends JFilter {
            @Override
            public Set<Integer>[] filterJava(Set<Integer>[] variables) {
                return filteringAllDifferentBC(variables);
            }
        }
        // creation of the two filtering algorithms to be compared
        Filter trusted = new BoundZFiltering(Checkers.allDifferent());
        Filter tested = new MyFilter();

        // setting the test parameters
        CPChecker.testArguments().setRangeForAll(-5, 5);
        CPChecker.testArguments().setNbTests(1000);
        CPChecker.testArguments().setSeed(100);

        // checking that the trusted and tested filtering return the same domains over
        // some random test instances
        CPChecker.check(trusted, tested,
                CPChecker.testArguments(), CPChecker.stats());
    }


    /*
     * The jacop's bound(Z) filtering for the allDifferent constraint
     */
    static private Set<Integer>[] filteringAllDifferentBC(Set<Integer>[] variables) {
        Store store = new Store();
        IntVar[] jacopVars = toIntVar(variables, store);
        Alldiff constraint = new Alldiff(jacopVars);
        store.impose(constraint);
        try {
            constraint.consistency(store);
            if (!store.consistency()) throw new NoSolutionException("no solutions");
            //store.imposeWithConsistency(constraint);
        } catch (FailException e) {
            throw new NoSolutionException("fail exception");
        }
        return toSets(jacopVars);
    }

    /*
     * Transforming jacop's IntVar type into Set<Integer>[] type
     */
    static private Set<Integer>[] toSets(IntVar[] jacopVars) {
        Set<Integer>[] variables = new Set[jacopVars.length];
        for (int i = 0; i < jacopVars.length; i++) {
            variables[i] = new HashSet<Integer>();
            int[] v = jacopVars[i].domain.toIntArray();
            for (int j = 0; j < v.length; j++) {
                variables[i].add(v[j]);
            }
        }
        return variables;
    }

    /*
     * Transforming Set<Integer>[] type into jacop's IntVar[] type
     */
    static private IntVar[] toIntVar(Set<Integer>[] vars, Store store) {
        IntVar[] jacopVars = new IntVar[vars.length];
        for (int i = 0; i < vars.length; i++) {
            jacopVars[i] = new IntVar(store);
            for (Integer value : vars[i]) {
                jacopVars[i].addDom(value, value);
            }
        }
        return jacopVars;
    }

}
