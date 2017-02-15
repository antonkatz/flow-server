import com.earldouglas.xwp.JettyPlugin
import org.scalatra.sbt._
import org.scalatra.sbt.PluginKeys._
import org.fusesource.scalate.ScalatePlugin.ScalateKeys._
import org.fusesource.scalate.ScalatePlugin.{Binding, TemplateConfig}

val ScalatraVersion = "2.5.0"

ScalatraPlugin.scalatraSettings

scalateSettings

organization := "in.flow"

name := "flow server"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.1"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.5" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.2.15.v20160210" % "container",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",

  "org.scalatra" %% "scalatra-json" % ScalatraVersion,
  "org.json4s"   %% "json4s-jackson" % "3.5.0",
  "org.scalatra" %% "scalatra-commands" % ScalatraVersion,

  "org.bouncycastle" % "bcprov-jdk15on" % "1.56",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.56",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalateTemplateConfig in Compile := {
  val base = (sourceDirectory in Compile).value
  Seq(
    TemplateConfig(
      base / "webapp" / "WEB-INF" / "templates",
      Seq.empty,  /* default imports should be added here */
      Seq(
        Binding("context", "_root_.org.scalatra.scalate.ScalatraRenderContext", importMembers = true, isImplicit = true)
      ),  /* add extra bindings here */
      Some("templates")
    )
  )
}

enablePlugins(JettyPlugin)

javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"

javaOptions in Jetty += "-agentpath:/opt/jrebel/lib/libjrebel64.so"

jrebelSettings

jrebel.webLinks += (sourceDirectory in Compile).value / "webapp"

jrebel.enabled := true