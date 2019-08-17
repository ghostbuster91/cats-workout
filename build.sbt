lazy val commonSettings = commonSmlBuildSettings ++ acyclicSettings ++ Seq(
  organization := "com.sakiewka",
  scalaVersion := "2.12.8",
  scalafmtOnCompile := true
)

name := "cats-workout"

version := "0.1"

scalaVersion := "2.12.7"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "cats-workout")
  .aggregate(chapter1)

lazy val chapter1: Project = (project in file("chapter1"))
  .settings(commonSettings: _*)
  .settings(
    name := "chapter1",
    libraryDependencies ++= Seq(
      compilerPlugin("com.softwaremill.neme" %% "neme-plugin" % "0.0.2"),
    )
  )
