name := "GenericChecker"

version := "1.0"


scalaVersion := "2.12.6"

resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot/"

updateOptions := updateOptions.value.withLatestSnapshots(false) // to be added because of a bad behaviour in the sbt version

libraryDependencies += "oscar" %% "oscar-cp" % "4.0.0-SNAPSHOT" withSources()
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.6"
libraryDependencies += "org.choco-solver" % "choco-solver" % "4.0.5"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
// https://mvnrepository.com/artifact/org.jacop/jacop
libraryDependencies += "org.jacop" % "jacop" % "4.4.0"
libraryDependencies += "org.assertj" % "assertj-core" % "3.10.0"

unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "main" / "examples" / "java"

unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "main" / "examples" / "scala"