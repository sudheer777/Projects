name := "project"

version := "0.1"

scalaVersion := "2.12.6"

val AkkaHttpVersion = "10.1.3"
libraryDependencies ++= Seq(
  "org.apache.httpcomponents" % "httpclient" % "4.5.2",
  "org.json4s" %% "json4s-jackson" % "4.0.3",
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion,
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
