import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "morizon",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "LoanCalculator",
    libraryDependencies += scalaTest % Test
  )
