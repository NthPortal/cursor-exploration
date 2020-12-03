import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "lgbt.princess"

lazy val root = (project in file("."))
  .settings(
    name := "cursor-exploration",
    libraryDependencies += scalaTest % Test
  )
