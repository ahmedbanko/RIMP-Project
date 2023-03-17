ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.14"

lazy val root = (project in file("."))
  .settings(
    name := "RIMP-Project"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.14"