organization := "in.flow"

name := "flow server"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.1"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.3",
  "io.spray" %%  "spray-json" % "1.3.3",
  "ch.qos.logback" % "logback-classic" % "1.1.5" % "runtime",

  "org.bouncycastle" % "bcprov-jdk15on" % "1.56",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.56",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.3" % "test"
)

//javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"

//javaOptions in Jetty += "-agentpath:/opt/jrebel/lib/libjrebel64.so"

//jrebelSettings
//
//jrebel.webLinks += (sourceDirectory in Compile).value / "webapp"
//
//jrebel.enabled := true