package checker;

import java.util.Set;

public class Pop extends BranchOp{
    public Pop(Set<Integer>[] domains){super(domains);}
    @Override
    public BranchOp clone(){
        return new Pop(domains.clone());
    }
    @Override
    public String toString() {
        return "Pop\n";
    }
}