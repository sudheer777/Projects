name := "majorly-meteoric"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "com.amazonaws" % "aws-java-sdk-s3" % "1.11.908",
  "org.json4s" %% "json4s-jackson" % "3.2.11",
  "org.apache.hadoop" % "hadoop-aws" % "2.8.5",
  "org.apache.spark" %% "spark-core" % "2.4.3",
  "org.apache.spark" %% "spark-sql" % "2.4.3",
)
