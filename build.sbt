name := "vlinderc"

version := "0.0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "com.google.code.gson" % "gson" % "2.3.1",
  "com.google.guava" % "guava" % "18.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

test in assembly := {}

mainClass in assembly := Some("org.vlinderlang.vlinderc.Main")
