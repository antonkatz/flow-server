organization := "in.flow"

name := "flow server"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.1"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.3",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.3",
  "io.spray" %%  "spray-json" % "1.3.3",
  "com.wix" %% "accord-core" % "0.6.1",

  "ch.qos.logback" % "logback-classic" % "1.1.5" % "runtime",
  "org.slf4j" % "slf4j-api" % "1.7.24",

  "org.bouncycastle" % "bcprov-jdk15on" % "1.56",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.56",

  "org.postgresql" % "postgresql" % "9.4.1212",
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.0",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.3" % "test"
)