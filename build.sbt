name := "FirstPrototype"

version := "1.0"

scalaVersion := "2.11.4"

resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot/"

libraryDependencies += "oscar" %% "oscar-cp" % "4.0.0-SNAPSHOT" withSources()


