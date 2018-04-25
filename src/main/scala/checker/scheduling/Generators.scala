package checker.scheduling

import org.scalacheck.Gen

/**
  * This Object contains all the generators used for the random tests of the constraints
  */
object Generators {
  val activity: Gen[Activity] = for {
    starts: Set[Int] <- Gen.containerOfN[Set, Int](3, Gen.choose(0, 200))
    duration: Set[Int] <- Gen.containerOfN[Set, Int](1, Gen.choose(1, 15))
  } yield new Activity(starts, duration, starts.map(_ + duration.last))

  val scheduling: Gen[List[Activity]] = Gen.containerOfN[List, Activity](10, activity)
}
