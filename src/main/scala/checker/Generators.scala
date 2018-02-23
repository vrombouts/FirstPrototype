package checker

import org.scalacheck.Gen

object Generators {
  val biDomain: Gen[Set[Int]] =  Gen.containerOfN[Set,Int](2,Gen.choose(-10,10))
  val triDomain:Gen[Set[Int]] =  Gen.containerOfN[Set,Int](3,Gen.choose(-10,10))
  def arrayGen(size: Int):Gen[Array[Int]] =  Gen.containerOfN[Array,Int](size,Gen.choose(-10,10))

  val basic: Gen[List[Set[Int]]] = Gen.containerOfN[List,Set[Int]](8,biDomain)

  val table: Gen[(List[Set[Int]], Set[Array[Int]])] = for {
    variables <- Gen.containerOfN[List,Set[Int]](8,biDomain)
    table <- Gen.nonEmptyContainerOf[Set,Array[Int]](arrayGen(variables.size))
  } yield (variables,table)

  val element: Gen[(List[Set[Int]],Set[Int],Set[Int])] = for {
    variables <- Gen.containerOfN[List,Set[Int]](20,Gen.containerOfN[Set,Int](3,Gen.choose(0,20)))
    k <- Gen.choose(5,15)
    l <- Gen.choose(1,7)
    i <- Gen.containerOfN[Set,Int](k,Gen.choose(0,20))
    v <-  Gen.containerOfN[Set,Int](l,Gen.choose(0,20))
  }yield (variables,i,v)

  val sum: Gen[List[Set[Int]]] = Gen.containerOfN[List,Set[Int]](20,biDomain)
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
