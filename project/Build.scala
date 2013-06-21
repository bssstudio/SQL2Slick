import sbt._
import sbt.Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object SqlToSlickBuild extends Build {
  val baseSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.10.0",
    scalacOptions ++= Seq("-feature", "-deprecation")
  )

  lazy val common = Project(
    id = "sql2slick",
    base = file("."),
    settings = baseSettings ++ Seq(
      name := "sql2slick",
      organization := "si.bss.tools.sql2slick",
      version := "0.1-SNAPSHOT",
      resolvers ++= Seq(
        "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"
      ),
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "1.9.1" % "test"
      )
    ) ++ assemblySettings
  )
}

