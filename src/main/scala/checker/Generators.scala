package checker

import org.scalacheck.Gen

/**
  * This Object contains all the generators used for the random tests of the constraints
  */
object Generators {
  val biDomain: Gen[Set[Int]] =  Gen.containerOfN[Set,Int](2,Gen.choose(-10,10))
  val triDomain:Gen[Set[Int]] =  Gen.containerOfN[Set,Int](3,Gen.choose(-10,10))
  def arrayGen(size: Int):Gen[Array[Int]] =  Gen.containerOfN[Array,Int](size,Gen.choose(-10,10))

  val basic: Gen[List[Set[Int]]] = Gen.containerOfN[List,Set[Int]](20,biDomain)

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

  val sum: Gen[List[Set[Int]]] = Gen.containerOfN[List,Set[Int]](20,triDomain)

  //useless
  def GeneratorVariable(): Gen[Variable] ={
    for{
      set <- biDomain
    }yield new Variable(set)
  }
  def GeneratorListOfVariables(n:Int): Gen[List[Variable]]={
    Gen.containerOfN[List,Variable](21,GeneratorVariable())
  }
}
