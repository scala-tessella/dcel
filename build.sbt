import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.2"

// Common settings for all platforms
lazy val commonSettings = Seq(
  idePackagePrefix := Some("io.github.scala_tessella"),
  libraryDependencies ++= Seq(
    "io.github.scala-tessella" %%% "ring-seq" % "0.6.2",
    "org.typelevel" %%% "spire" % "0.18.0",
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test
  )
)

// JVM-specific settings
lazy val jvmSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "2.4.0"
  )
)

// JS-specific settings
lazy val jsSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-xml" % "2.4.0"
  ),
  scalaJSUseMainModuleInitializer := true,
  // Change module kind to CommonJS for Node.js compatibility
  scalaJSLinkerConfig ~= {
    _.withModuleKind(ModuleKind.CommonJSModule)
  },
  // Use Node.js for testing
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // Optional: if you want to use main module initializer
  scalaJSUseMainModuleInitializer := false
)

// Cross-platform project definition
lazy val dcel = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name := "dcel"
  )
  .settings(commonSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)

// Individual platform projects
lazy val dcelJVM = dcel.jvm
lazy val dcelJS = dcel.js

// Aggregate root project for IDE compatibility
lazy val root = project
  .in(file("."))
  .aggregate(dcelJVM, dcelJS)
  .settings(
    name := "dcel-root",
    publish := {},
    publishLocal := {}
  )