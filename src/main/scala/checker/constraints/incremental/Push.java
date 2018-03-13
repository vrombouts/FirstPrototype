package checker.constraints.incremental;


import java.util.Set;

public class Push extends BranchOp {
    public Push(Set<Integer>[] domains){super(domains);}
    @Override
    public BranchOp clone(){
        return new Push(domains.clone());
    }
    @Override
    public String toString() {
        return "Push\n";
    }
}

