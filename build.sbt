name := "AirportReports"
version := "1.10.7"
ThisBuild / scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.4.1",
  "com.h2database" % "h2" % "2.2.224",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.4.1",
  "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2",
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)