
import sbt._
import Keys._
import java.io.File



object BuildSettings {

  val buildSettings = Defaults.defaultSettings ++ Seq (
    version := "0.2",
    scalaVersion := "2.11.0",
    scalacOptions ++= Seq("-deprecation", "-optimise"),
    logBuffered := false,
    initialCommands in console := "import scala.collection.optimizer._",
    organization := "com.github.scala-blitz"
  )

  val rootSettings = buildSettings ++ Seq (
    name := "scala-blitz-pviews"
  )
}


object BlitzViewBuild extends Build {
  /* projects */
  lazy val root = Project(
    "root",
    file("."),
    settings = BuildSettings.rootSettings
  ) dependsOn (
      uri("git://github.com/axel-angel/scala-blitzview.git#3b859fda380e52dcc557bb88abe89afed69eb393")
  )
}










