package jacop;

import org.jacop.core.*;
import org.jacop.constraints.*;
import org.jacop.search.*;

import java.util.HashSet;
import java.util.Set;

public class Test {

    static public void main(String[] args){
        Store store = new Store();
        //IntVar[] jacopVars = new IntVar[vars.length];
        //for(int i=0; i<vars.length;i++){
        //    jacopVars[i] = toIntVar(vars[i], store);
        //}
        //SumInt constraint = new SumInt(store, jacopVars, "==", new IntVar(store, "constant", 15,15));

        IntVar[] aa = new IntVar[3];
        aa[0] = new IntVar(store, "o", 1,2);
        aa[1] = new IntVar(store, "p", 1,2);
        aa[2] = new IntVar(store, "q", 1,3);
        Alldifferent ab= new Alldifferent(aa);
        store.impose(ab);
        System.out.println(aa[0]+ "\n" + aa[1] + "\n" + aa[2]);

    }

    static public IntVar toIntVar(Set<Integer> ss,Store store){
        IntVar var = new IntVar(store);
        for(Integer s : ss){
            var.addDom(s,s);
        }
        return var;

    }
}
