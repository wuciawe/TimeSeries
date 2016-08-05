name := "TimeSeries"

version := "1.0"

crossScalaVersions := Seq("2.10.6", "2.11.8")

scalaVersion := crossScalaVersions.value.last

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

// https://mvnrepository.com/artifact/org.apache.commons/commons-math3
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
