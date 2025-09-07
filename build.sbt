ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.2"
ThisBuild / organization := "io.github.scala-tessella"

// Enable semanticdb for Scalafix (Scala 3)
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

// Optional: format when compiling (can be noisy in PRs; turn off if you prefer manual runs)
ThisBuild / scalafmtOnCompile := true

// Optional: add a convenient alias to lint/format/test locally
addCommandAlias("qa", ";scalafmtAll;test:scalafmtAll;scalafixAll;test")

// Common settings for all platforms
lazy val commonSettings = Seq(
//  idePackagePrefix := Some("io.github.scala_tessella"),
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
    "-deprecation",         // warn on deprecated APIs
    "-feature",             // warn on feature imports/usages
    "-unchecked",           // extra checks for pattern matches, etc.
    "-Wvalue-discard",      // warn when a non-Unit value is ignored
    "-Wnonunit-statement",  // warn on statements that return non-Unit
    "-Wunused:imports"      // needed by Scalafix to use OrganizeImports.removeUnused
  ),
  // Apply fatal warnings only for main (Compile), not for tests
  Compile / scalacOptions ++= Seq(
    "-Xfatal-warnings"
  ),
  Test / scalacOptions --= Seq(
    "-Xfatal-warnings"
  ),
  // Optional: keep the REPL (console) friendly by stripping fatal warnings there too
  Compile / console / scalacOptions --= Seq("-Xfatal-warnings"),
  Test    / console / scalacOptions --= Seq("-Xfatal-warnings"),
    // Optional: wartremover – start with a conservative set for Scala 3
  wartremoverErrors ++= Seq(
//    wartremover.Wart.Any,
//    wartremover.Wart.Null,
//    wartremover.Wart.Var,
    wartremover.Wart.AsInstanceOf,
    wartremover.Wart.IsInstanceOf,
//    wartremover.Wart.OptionPartial,
//    wartremover.Wart.Throw
  ),
  // You can relax in Test scope (warnings instead of errors)
  Test / wartremoverWarnings ++= Seq(
    wartremover.Wart.Any
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