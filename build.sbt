name := "vlinderc"

version := "0.0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalaz" %% "scalaz-core" % "7.1.3"
)
