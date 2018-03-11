package checker.incremental;

import checker.Op;

import java.util.*;

public class RestrictDomain extends BranchOp {
    private Random random = new Random();
    public int index;
    public int constant;
    public String op;
    public RestrictDomain(Set<Integer>[] domains){
        super(domains);
        this.random = new Random();
        this.index = random.nextInt(domains.length);
        this.op = Op.randomOp();
        this.constant = randomConstant();
    }

    private int randomConstant(){
        Set<Integer> dom = domains[index];
        while (dom.size()<2) {
            index = random.nextInt(domains.length);
            dom=super.domains[index];
        }
        if      (op.equals(Op.lesserThan()) || op.equals(Op.greaterThanOrEqual())) dom =min(dom);
        else if (op.equals(Op.greaterThan()) || op.equals(Op.lesserThanOrEqual())) dom = max(dom);
        Integer[] variable = dom.toArray(new Integer[dom.size()]);
        return variable[random.nextInt(variable.length)];
    }


    public Set<Integer>[] applyRestriction(){
        Set<Integer> domainToReduced= new HashSet<>(domains[index]);
        for(Integer i: domains[index]) {
            if (!Op.respectOp(op, i, constant)) domainToReduced.remove(i);
        }
        domains[index]=domainToReduced;
        return domains;
    }



    private Set<Integer> min(Set<Integer> dom){
        Integer minimum = Collections.min(dom);
        Set<Integer> trunc = new HashSet<>();
        for(Integer i : dom){
            if(!minimum.equals(i) )trunc.add(i);
        }
        return trunc;
    }
    private Set<Integer> max(Set<Integer> dom){
        Integer maximum =  Collections.max(dom);
        Set<Integer> trunc = new HashSet<>();
        for(Integer i : dom){
            if(!maximum.equals(i) )trunc.add(i);
        }
        return trunc;
    }

    @Override
    public BranchOp clone(){
        RestrictDomain rd = new RestrictDomain(domains.clone());
        rd.index    = this.index;
        rd.constant = this.constant;
        rd.op = this.op;
        return rd;
    }

    @Override
    public String toString() {
        return "Restriction of domains: x_"+index+op+constant+"\n";
    }
}
