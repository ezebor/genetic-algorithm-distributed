ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "genetic-algorithm-distributed"
  )

val akkaVersion = "2.7.0"
val scalaTestVersion = "3.2.14"

// https://github.com/protocolbuffers/protobuf/releases
// protoc-21.12-osx-x86_64.zip
lazy val protobufVersion = "3.21.12"

libraryDependencies ++= Seq(
  // testing
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion,

  // clustering and remoting
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-sharding" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
  "io.aeron" % "aeron-driver" % "1.39.0",
  "io.aeron" % "aeron-client" % "1.39.0",
  "io.spray" %%  "spray-json" % "1.3.6",

  // serialization
  "com.google.protobuf" % "protobuf-java" % protobufVersion,
)