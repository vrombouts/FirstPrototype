package checker;

import java.util.Set;

public class BranchOp {
    public Set<Integer>[] domains;
    public BranchOp(Set<Integer>[] domains){ this.domains=domains;}

    @Override
    public BranchOp clone(){
        return new BranchOp(domains.clone());
    }

    @Override
    public String toString() {
        return "No more branching possible";
    }
}



