import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.larskroll",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "NMEA Parser",
    libraryDependencies ++= jvmDeps.value,
    libraryDependencies ++= testDeps.value
  )
