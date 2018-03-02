package checker

import org.scalacheck.Gen

/**
  * This Object contains all the generators used for the random tests of the constraints
  */
object Generators {
  val biDomain: Gen[Set[Int]] =  Gen.containerOfN[Set,Int](2,Gen.choose(-10,10))
  val triDomain:Gen[Set[Int]] =  Gen.containerOfN[Set,Int](3,Gen.choose(-10,10))
  def arrayGen(size: Int):Gen[Array[Int]] =  Gen.containerOfN[Array,Int](size,Gen.choose(-10,10))

  val basic: Gen[List[Set[Int]]] = Gen.containerOfN[List,Set[Int]](10,triDomain)

  val gcc: Gen[List[Set[Int]]] = for{
    three       <- Gen.containerOfN[Set,Int](2,Gen.choose(0,2))
    assignments <- Gen.containerOfN[List,Set[Int]](8,three)
    ten   <- Gen.containerOfN[Set,Int](2,Gen.choose(0,8))
    count <- Gen.containerOfN[List,Set[Int]](3,ten)
  }yield assignments++count

  val table: Gen[(List[Set[Int]], Set[Array[Int]])] = for {
    variables <- basic
    table <- Gen.nonEmptyContainerOf[Set,Array[Int]](arrayGen(variables.size))
  } yield (variables,table)

  val element: Gen[(List[Set[Int]],Set[Int],Set[Int])] = for {
    variables <- Gen.containerOfN[List,Set[Int]](20,triDomain)
    k <- Gen.choose(5,15)
    l <- Gen.choose(1,7)
    i <- Gen.containerOfN[Set,Int](k,Gen.choose(0,20))
    v <-  Gen.containerOfN[Set,Int](l,Gen.choose(-10,10))
  }yield (variables,i,v)

  def computeMin(l:List[Set[Int]]) : (Int)={
    var resMin:Int=0
    for(l1 <- l){
      resMin+=l1.min
    }
    resMin
  }

  def computeMax(l:List[Set[Int]]) : (Int)={
    var resMax:Int=0
    for(l1 <- l){
      resMax+=l1.max
    }
    resMax
  }

  val sum: Gen[(List[Set[Int]], Int)] = for{
    variables <- Gen.containerOfN[List,Set[Int]](20,Gen.containerOfN[Set,Int](3,Gen.choose(0,10)))
    sumMin <- Gen.choose(computeMin(variables),computeMin(variables))
    sumMax <- Gen.choose(computeMax(variables),computeMax(variables))
    s <- Gen.choose(sumMin-5,sumMax+5) suchThat( s => s<sumMin+(sumMax-sumMin)/6 || s>sumMax-(sumMax-sumMin)/6)
  }yield (variables,s)
  // maybe test with variables of different domains lengths (1,2,3...)

}
