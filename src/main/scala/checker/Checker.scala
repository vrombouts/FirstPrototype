package checker
import  scala.util.Random
import scala.language.implicitConversions

trait Checker {
  def checkEmpty(variables: List[Set[Int]]): Boolean = {
    variables.foreach{x => if(x.isEmpty) return true}
    false
  }

  def printer(initial:Array[Set[Int]],trueReduced:Array[Set[Int]],reduced:Array[Set[Int]],error:Boolean,ourError:Boolean):Unit = {
    if(error && !ourError) {
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you returned an exception")
    }else if(!error && ourError){
      println("failed for: " + initial.toList)
      println("you should not have any solutions")
      println("but you had: " + reduced.toList)
    }else if(!error && !ourError){
      println("failed for: " + initial.toList)
      println("you should have: " + trueReduced.toList)
      println("but you had: " + reduced.toList)
    }
  }

  def applyConstraint(variables: Array[Set[Int]]): Array[Set[Int]]

  def applyConstraint(b:BranchOp): Array[Set[Int]]

  def checkConstraint(variables:Array[Set[Int]],
                      constraintTested:Array[Set[Int]]=>Array[Set[Int]])
  : Boolean ={
    //We first compute the domains generated after the application of the constraint.
    var reducedDomains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var ourError:Boolean = false
    try {
      reducedDomains = constraintTested(variables.clone())
    }
    catch{
      //TODO check if it is not better to have a case of NoSolutionException instead
      case _: NoSolutionException => error = true
    }
    // Then we generate the domains that reducedDomains should have
    var trueReducedDomains: Array[Set[Int]] = Array()
    try {
      trueReducedDomains = applyConstraint(variables.clone())
    }
    catch {
      case _: NoSolutionException => ourError = true
    }
    //Finally, we compare the two. If they are not equals, the constraint is not correct.
    if(error && ourError) return true

    if(error && !ourError){
      for(i<- trueReducedDomains.indices){
        if(trueReducedDomains(i).nonEmpty){
          printer(variables,trueReducedDomains,reducedDomains,error,ourError)
          return false
        }
      }
    }
    else if(!ourError) {
      for (i <- trueReducedDomains.indices) {
        if (!trueReducedDomains(i).equals(reducedDomains(i))) {
          printer(variables,trueReducedDomains,reducedDomains,error,ourError)
          return false
        }
      }
    }
    else{
      //empty domains accepted as having no solutions
      if(reducedDomains.nonEmpty && reducedDomains.forall(x=> x.nonEmpty)){
        printer(variables,trueReducedDomains,reducedDomains,error,ourError)
        return false
      }
    }
    true
  }

  def checkConstraint(variables:Array[Set[Int]],
                      init:Array[Set[Int]] => Array[Set[Int]],
                      constraintTested:BranchOp=>Array[Set[Int]])
  : Boolean = {
    //We first compute the domains generated after the application of the constraint.
    var reducedDomains: Array[Set[Int]] = Array()
    var error: Boolean = false
    var ourError: Boolean = false
    try {
      reducedDomains = init(variables.clone())
    }
    catch {
      //TODO check if it is not better to have a case of NoSolutionException instead
      case e: NoSolutionException => error = true
    }
    // Then we generate the domains that reducedDomains should have
    var ourReducedDomains: Array[Set[Int]] = Array()
    try {
      ourReducedDomains = applyConstraint(variables.clone())
    }
    catch {
      case e: NoSolutionException => ourError = true
    }
    if(!comparison(error,ourError,variables,reducedDomains,ourReducedDomains))
      return false
    var vars:Array[Set[Int]]=ourReducedDomains.clone()
    var nPush:Int = 0
    var nPop:Int = 0
    val random = new Random
    val restrictDomainWeight=4
    for(i<- 0 until 20){
      // creation of the table of operations with the operations that are allowed
      // a Pop is not allowed if no push and a restrictDomain is not allowed if all variables are fixed to a value
      var operations:Array[BranchOp] = Array(new Push(vars))
      if(nPush > nPop)
        operations = operations :+ new Pop(vars)
      if(!allFixed(vars)) {
        // give more weight to the RestrictDomain operation since it allows multiple operations (<,>,=,!=)
        for(j <- 0 until restrictDomainWeight)
          operations = operations :+ new RestrictDomain(vars)
      }
      var indexOp = random.nextInt(operations.length)
      var b:BranchOp=operations(indexOp)
      if(b.isInstanceOf[Push]) nPush+=1
      else if(b.isInstanceOf[Pop]) nPop+=1

      // apply our constraint and the constraint of the user for the branchOp b and the domains vars
      var reducedDomains: Array[Set[Int]] = Array()
      var error: Boolean = false
      var ourError: Boolean = false
      try {
        reducedDomains = constraintTested(b)
      }
      catch {
        //TODO check if it is not better to have a case of NoSolutionException instead
        case e: NoSolutionException => error = true
      }
      // Then we generate the domains that reducedDomains should have
      var ourReducedDomains: Array[Set[Int]] = Array()
      try {
        ourReducedDomains = applyConstraint(b)
      }
      catch {
        case e: NoSolutionException => ourError = true
      }
      // compare our domains filtered with the ones of the user
      // and if no difference, update the doamains for the next branching operation
      if(comparison(error,ourError,vars,reducedDomains,ourReducedDomains))
        vars=ourReducedDomains.clone()
      else
        return false
    }
    true
  }

  def allFixed(variables:Array[Set[Int]]) : Boolean = {
    !variables.exists(x => x.size != 1)
  }

  def comparison(error:Boolean, ourError:Boolean, variables:Array[Set[Int]], reducedDomains:Array[Set[Int]], ourReducedDomains:Array[Set[Int]]) : Boolean = {
    //Finally, we compare the two. If they are not equals, the constraint is not correct.
    if(error && ourError) return true

    if(error && !ourError){
      for(i<- ourReducedDomains.indices){
        if(ourReducedDomains(i).nonEmpty){
          printer(variables,ourReducedDomains,reducedDomains,error,ourError)
          return false
        }
      }
    }
    else if(!ourError) {
      for (i <- ourReducedDomains.indices) {
        if (!ourReducedDomains(i).equals(reducedDomains(i))) {
          printer(variables,ourReducedDomains,reducedDomains,error,ourError)
          return false
        }
      }
    }
    else{
      //empty domains accepted as having no solutions
      if(reducedDomains.nonEmpty && reducedDomains.forall(x=> x.nonEmpty)){
        printer(variables,ourReducedDomains,reducedDomains,error,ourError)
        return false
      }
    }
    true
  }
}