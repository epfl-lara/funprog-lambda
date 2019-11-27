ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.12.8"

resolvers += Resolver.bintrayRepo("epfl-lara", "maven")
libraryDependencies += "ch.epfl.lara" %% "scallion" % "0.4"

lazy val core = project
  .in(file("core"))
  .settings(
    name := "example",
  )
  .dependsOn(verified)

lazy val verified = project
  .in(file("verified"))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := "example-verified"
  )

lazy val recursive = project
  .in(file("recursive"))
//  .enablePlugins(StainlessPlugin)
  .settings(
    name := "recursive"
  )
