lazy val commonSettings = commonSmlBuildSettings ++ acyclicSettings ++ Seq(
  organization := "com.sakiewka",
  scalaVersion := "2.12.8",
)

name := "cats-workout"

version := "0.1"

scalaVersion := "2.12.8"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "cats-workout")
  .aggregate(chapter1, chapter2, macroTest)

lazy val chapter1: Project = (project in file("chapter1"))
  .settings(commonSettings: _*)
  .settings(
    name := "chapter1",
    libraryDependencies ++= Seq(
      compilerPlugin("com.softwaremill.neme" %% "neme-plugin" % "0.0.2"),
      "org.typelevel" %% "cats-core" % "1.0.0"
    )
  ).dependsOn(macroTest)


lazy val chapter2: Project = (project in file("chapter2"))
  .settings(commonSettings: _*)
  .settings(
    name := "chapter1",
    libraryDependencies ++= Seq(
      compilerPlugin("com.softwaremill.neme" %% "neme-plugin" % "0.0.2"),
      "org.typelevel" %% "cats-core" % "1.0.0"
    )
  ).dependsOn(macroTest)


lazy val macroTest: Project = (project in file("macrotest"))
  .settings(commonSettings: _*)
  .settings(
    name := "chapter1",
    libraryDependencies ++= Seq(
      compilerPlugin("com.softwaremill.neme" %% "neme-plugin" % "0.0.2"),
      "org.typelevel" %% "cats-core" % "1.0.0"
    )
  )
