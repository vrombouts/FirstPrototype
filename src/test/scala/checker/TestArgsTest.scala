package checker

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec

import scala.util.Random

class TestArgsTest extends FlatSpec {

  var gen = new TestArgs
  gen.random.setSeed(10000)

  "gen" should "not have a set seed when initialized" in {
    assert(gen.getSeed != gen.getSeed)
  }

  "gen" should "be able to set its seed" in {
    gen.setSeed(1)
    assert(gen.getSeed == 1)
  }


  "TestArgs as basis" should "give a gen returning 5 variables with domains of max size 4 in ranges (-10,10)" in {
    val gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(100)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.size == 5 && list.forall(x => x.size <= 4 && x.forall { y => y <= 10 && -10 <= y }))
      case None => assert(false)
    }
  }

  "addVar" should "add a variable to the generated samples" in {
    gen.addVar()
    val gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.size == 6)
      case None => assert(false)
    }
  }

  "addNVar" should "add N (or 0 if N<0) variables to the generated samples" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.addNVar(-1)
    val g: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.size == 5)
      case None => assert(false)
    }
    gen.addNVar(10)
    val gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.size == 15)
      case None => assert(false)
    }
  }

  "addVar and addNVar " should "accept a density and range for the variables to add" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.addVar(0.5, (1, 4))
    val g: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.size == 6 && list.last.size == 2)
      case None => assert(false)
    }
    gen.addNVar(5, 0.6, (0, 3))
    val gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.size == 11 && list.drop(6).forall { x => x.size == 2 || x.size == 1 })
      case None => assert(false)
    }


    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.addVar(0.5, 1, 4)
    val ggg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    ggg match {
      case Some(list) =>
        assert(list.size == 6 && list.last.size == 2)
      case None => assert(false)
    }
    gen.addNVar(5, 0.6, 0, 3)
    val gggg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gggg match {
      case Some(list) =>
        assert(list.size == 11 && list.drop(6).forall { x => x.size == 2 || x.size == 1 })
      case None => assert(false)
    }
  }




  "setNVar" should "reset the number of variables to n (if n<1 => 1 variable)" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.setNVar(-5)
    val g: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.size == 1)
      case None => assert(false)
    }
    gen.setNVar(15)
    val gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.size == 15)
      case None => assert(false)
    }
  }

  "setDensity" should "set the density of the given variable" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.setDensity(0, 0.5)
    var g: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.head.size == 10 || list.head.size == 9)
      case None => assert(false)
    }
    gen.setDensity(0, 2.0)
    g = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.head.size == 10 || list.head.size == 9)
      case None => assert(false)
    }
    gen.setDensity(0, -2.0)
    g = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.head.size == 10 || list.head.size == 9)
      case None => assert(false)
    }
    gen.setDensityForAll(0.5)
    assert(gen.baseDensity == 0.5)
    var gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.forall { x => x.size <= 10 && x.size >= 8 })
      case None => assert(false)
    }
    gen.setDensityForAll(-2.0)
    gg = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.forall { x => x.size <= 10 && x.size >= 8 })
      case None => assert(false)
    }
    gen.setDensityForAll(2.0)
    gg = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.forall { x => x.size <= 10 && x.size >= 8 })
      case None => assert(false)
    }
  }


  "setRange" should "set the range of the asked variable" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.setRange(0, (100, 120))
    val g: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.head.forall(x => x <= 120 && x >= 100))
      case None => assert(false)
    }
    gen.reset()
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.setRange(0, 100, 120)
    val gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.head.forall(x => x <= 120 && x >= 100))
      case None => assert(false)
    }
  }

  "setRangeForAll" should "set the range of all the variables" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.setRangeForAll((100, 120))
    assert(gen.baseRange == (100, 120))
    val g: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.forall(vari => vari.forall(x => x <= 120 && x >= 100)))
      case None => assert(false)
    }

    gen.reset()
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.setRangeForAll(100, 120)
    assert(gen.baseRange == (100, 120))
    val gg: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    gg match {
      case Some(list) =>
        assert(list.forall(vari => vari.forall(x => x <= 120 && x >= 100)))
      case None => assert(false)
    }
  }

  "remove" should "remove the told variable in the generator" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    gen.setRange(0, (100, 120))
    gen.remove(2000)
    gen.remove(0)
    val g: Option[List[Set[Int]]] = gen.gen.apply(new Gen.Parameters {
      override val rng: Random = new Random(1000)
      override val size: Int = 100
    })
    g match {
      case Some(list) =>
        assert(list.size == 4 && list.forall(vari => !vari.exists(x => x >= 100 && x <= 120)))
      case None => assert(false)
    }

  }

  "randomSeed" should "unfixed the seed" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    val str1 = gen.toString
    gen.randomSeed()
    assert(!str1.contains("random") && gen.toString.contains("random"))
  }

  "getTestParameters" should "permit to run nbTests with the set seed using forAll" in {
    gen = new TestArgs
    gen.random.setSeed(10000)
    gen.setSeed(1)
    val gen2 = new TestArgs
    gen2.random.setSeed(10000)
    gen2.setSeed(1)
    var x: List[List[Set[Int]]] = List()
    var y: List[List[Set[Int]]] = List()
    forAll(gen.gen) { y => x = x :+ y; true }.check(gen.getTestParameters)
    forAll(gen2.gen) { x => y = y :+ x; true }.check(gen2.getTestParameters)
    assert((x zip y).forall(z => (z._1 zip z._2).forall(w => w._1.equals(w._2))))
    assert(gen.getNbTests==100)
    gen.setNbTests(10)
    assert(gen.getNbTests==10)
    var z = 0
    forAll(gen.gen) { y => z += 1; true }.check(gen.getTestParameters)
    assert(z == 10)
    gen = new TestArgs
    x = List()
    y = List()
    forAll(gen.gen) { y => x = x :+ y; true }.check(gen.getTestParameters)
    forAll(gen.gen) { x => y = y :+ x; true }.check(gen.getTestParameters)
    assert(x.size == 100 && y.size == 100 && !x.equals(y))
    gen.setNbTests(20)
    assert(gen.getNbTests==20)
    x = List()
    y = List()
    forAll(gen.gen) { y => x = x :+ y; true }.check(gen.getTestParameters)
    forAll(gen.gen) { x => y = y :+ x; true }.check(gen.getTestParameters)
    assert(x.size == 20 && y.size == 20 && !x.equals(y))

  }

}
