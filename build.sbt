name := "Betsy"

ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.16"
ThisBuild / organization := "org.example"

val spinalVersion = "1.9.4"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

lazy val projectname = (project in file("."))
  .settings(
    Compile / scalaSource := baseDirectory.value / "src" / "main" / "scala" ,
    // Compile / unmanagedSourceDirectories += baseDirectory.value / "software",  // the row compiler
    libraryDependencies ++= Seq(spinalCore,
      spinalLib,
      spinalIdslPlugin,
      "org.scalatest" %% "scalatest" % "3.2.5",
      "com.lihaoyi" %% "upickle" % "3.1.1"
    )
  )

fork := true
