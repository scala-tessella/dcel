ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.2"

// Common settings for all platforms
lazy val commonSettings = Seq(
  idePackagePrefix := Some("io.github.scala_tessella"),
  libraryDependencies ++= Seq(
    "io.github.scala-tessella" %%% "ring-seq" % "0.6.2",
    "org.typelevel" %%% "spire" % "0.18.0",
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    // ScalaCheck + ScalaTest integration for property-based tests (JVM & JS)
    "org.scalacheck" %%% "scalacheck" % "1.18.1" % Test,
    "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test
  ),
  // Compiler hygiene: turn on key warnings and make them fail the build
  scalacOptions ++= Seq(
    "-Xfatal-warnings",     // fail on warnings
    "-deprecation",         // warn on deprecated APIs
    "-feature",             // warn on feature imports/usages
    "-unchecked",           // extra checks for pattern matches, etc.
    "-Wvalue-discard",      // warn when non-Unit value is ignored
    "-Wnonunit-statement"   // warn on statements that return non-Unit
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
  // Use Node.js for testing
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // Use a stable default for library usage; set to true only if you expose a JS main
  scalaJSUseMainModuleInitializer := false,
  // Change module kind to CommonJS for Node.js compatibility
  scalaJSLinkerConfig ~= {
    _.withModuleKind(ModuleKind.CommonJSModule)
  }
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