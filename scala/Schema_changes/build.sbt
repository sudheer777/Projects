name := "jsonvalidator"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++=Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.apache.avro" % "avro" % "1.8.2",
  "com.typesafe.play" %% "play-json" % "2.9.2"
)