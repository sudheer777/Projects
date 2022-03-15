scalaVersion := "2.13.4"
//val zioVersion = "1.0"
val zioVersion = "2.0.0-M4"
//val zioVersion = "1.0.9"
//scalaVersion := "3.1.1"

resolvers += Resolver.sonatypeRepo("snapshots")

val grpcVersion = "1.41.2"

/*
PB.targets in Compile := Seq(
        scalapb.gen(grpc = true) -> (sourceManaged in Compile).value,
        scalapb.zio_grpc.ZioCodeGenerator -> (sourceManaged in Compile).va
lue,
    )

libraryDependencies ++= Seq(
        "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
        "io.grpc" % "grpc-netty" % grpcVersion
        "dev.zio" %% "zio" % "2.0.0-M4",
    )
*/

Compile / PB.targets := Seq(
  scalapb.gen(grpc = true) -> (Compile / sourceManaged).value,
  scalapb.zio_grpc.ZioCodeGenerator -> (Compile / sourceManaged).value
)

libraryDependencies ++= Seq(
  //"dev.zio" %% "zio" % "2.0.0-M4",
  "dev.zio" %% "zio" % "1.0.9",
  "io.grpc" % "grpc-netty" % grpcVersion,
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
)

run / fork := true
