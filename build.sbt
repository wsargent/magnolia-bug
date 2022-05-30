ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "magnolia-bug",
    libraryDependencies += "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.2",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.tersesystems.echopraxia" % "api" % "2.0.1"
  )
