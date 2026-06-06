ThisBuild / scalaVersion  := "3.8.4"
ThisBuild / organization  := "io.github.scala-tessella"
ThisBuild / versionScheme := Some("early-semver")

// Enable semanticdb for Scalafix (Scala 3)
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

// Publishing metadata (sbt-ci-release derives the version from git tags via sbt-dynver)
ThisBuild / licenses   := Seq(
  "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"),
  "MIT"        -> url("https://opensource.org/licenses/MIT")
)
ThisBuild / developers := List(Developer(
  "scala-tessella",
  "scala-tessella",
  "mario.callisto@gmail.com",
  url("https://github.com/scala-tessella")
))
ThisBuild / homepage   := Some(url("https://github.com/scala-tessella/dcel"))
ThisBuild / scmInfo    := Some(ScmInfo(
  url("https://github.com/scala-tessella/dcel"),
  "scm:git:git@github.com:scala-tessella/dcel.git"
))

// Optional: format when compiling (can be noisy in PRs; turn off if you prefer manual runs)
ThisBuild / scalafmtOnCompile := true

// Optional: add a convenient alias to lint/format/test locally
addCommandAlias("qa", ";scalafmtAll;test:scalafmtAll;scalafixAll;test")

// Common settings for all platforms
lazy val commonSettings = Seq(
//  idePackagePrefix := Some("io.github.scala_tessella"),
  libraryDependencies ++= Seq(
    "io.github.scala-tessella" %%% "ring-seq"        % "0.8.0",
    "io.github.iltotore"       %%% "iron"            % "3.2.3",
    "org.typelevel"            %%% "spire"           % "0.18.0",
    "org.scalatest"            %%% "scalatest"       % "3.2.19"   % Test,
    // ScalaCheck + ScalaTest integration for property-based tests (JVM & JS)
    "org.scalacheck"           %%% "scalacheck"      % "1.18.1"   % Test,
    "org.scalatestplus"        %%% "scalacheck-1-18" % "3.2.19.0" % Test
  ),
  // Compiler hygiene: turn on key warnings and make them fail the build
  scalacOptions ++= Seq(
    "-deprecation",        // warn on deprecated APIs
    "-feature",            // warn on feature imports/usages
    "-unchecked",          // extra checks for pattern matches, etc.
    "-Wvalue-discard",     // warn when a non-Unit value is ignored
    "-Wnonunit-statement", // warn on statements that return non-Unit
    "-Wunused:imports"     // needed by Scalafix to use OrganizeImports.removeUnused
  ),
  // Apply fatal warnings only for main (Compile), not for tests
  Compile / scalacOptions ++= Seq(
    "-Werror"
  ),
  Test / scalacOptions --= Seq(
    "-Werror"
  ),
  // Optional: keep the REPL (console) friendly by stripping fatal warnings there too
  Compile / console / scalacOptions --= Seq("-Xfatal-warnings"),
  Test / console / scalacOptions --= Seq("-Xfatal-warnings")
  // Optional: wartremover – start with a conservative set for Scala 3
//  wartremoverErrors ++= Seq(
////    wartremover.Wart.Any,
////    wartremover.Wart.Null,
////    wartremover.Wart.Var,
//    wartremover.Wart.AsInstanceOf,
//    wartremover.Wart.IsInstanceOf,
////    wartremover.Wart.OptionPartial,
////    wartremover.Wart.Throw
//  ),
  // You can relax in Test scope (warnings instead of errors)
//  Test / wartremoverWarnings ++= Seq(
//    wartremover.Wart.Any
//  )
)

// JVM-specific settings
lazy val jvmSettings = Seq(
  libraryDependencies ++= Seq(
//    "org.scala-lang.modules" %% "scala-xml" % "2.4.0"
  )
)

// JS-specific settings
lazy val jsSettings = Seq(
  libraryDependencies ++= Seq(
//    "org.scala-lang.modules" %%% "scala-xml" % "2.4.0"
  ),
  // Use Node.js for testing
  jsEnv                           := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // Use a stable default for library usage; set to true only if you expose a JS main
  scalaJSUseMainModuleInitializer := false,
  // Change module kind to CommonJS for Node.js compatibility
  scalaJSLinkerConfig ~= {
    _.withModuleKind(ModuleKind.CommonJSModule)
  }
)

// Cross-platform project definition.
// Scala Native is intentionally NOT wired in — see ADR-0007 ("Cross-platform targets"):
// Spire (a core dep) does not yet publish a Scala Native artifact, which is the blocker.
// The native/ directory and the commented-out NativePlatform / .nativeSettings / Native
// plugin lines are scaffolding that should stay until Spire unblocks.
lazy val dcel = crossProject(JVMPlatform, JSPlatform /*, NativePlatform — see ADR-0007*/ )
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name        := "dcel",
    description := "DCEL utilities for representing, building, editing and analysing edge-to-edge tessellations of unit-side polygons in Scala 3"
  )
  .settings(commonSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
//  .nativeSettings(  // blocked by Spire — see ADR-0007
//    // Add native-specific settings here
//  )

// Individual platform projects
lazy val dcelJVM = dcel.jvm
lazy val dcelJS  = dcel.js
// lazy val dcelNative = dcel.native  // re-enable together with the NativePlatform line above (ADR-0007)

// JMH benchmarks (opt-in: not aggregated by root, run via `benchmarks/Jmh/run ...`)
lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(dcelJVM)
  .settings(
    name             := "dcel-benchmarks",
    publish / skip   := true,
    Jmh / bspEnabled := false,
    // Keep benchmark compilation permissive; JMH-generated code can trip stricter flags.
    scalacOptions    := Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    )
  )

// Experimental n-uniform / n-archimedean tiling generator.
// Not part of the published artifact: the API and search heuristics are still
// in flux (research-grade code with placeholder names, magic-number cutters,
// and several disabled exploratory tests). Kept on main for iteration; run
// tests via `sbt generator/test`. Not aggregated by root — its tests are slow.
lazy val generator = project
  .in(file("generator"))
  .dependsOn(dcelJVM % "compile->compile;test->test")
  .settings(
    name           := "dcel-generator",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.18.1"   % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
    )
  )

// Aggregate root project for IDE compatibility + Scaladoc site publication.
// `sbt makeSite` packages the dcelJVM Scaladoc into target/site; the site.yml
// workflow deploys it to GitHub Pages on every `v*` tag push.
lazy val root = project
  .in(file("."))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .aggregate(dcelJVM, dcelJS)
  .settings(
    name                          := "dcel-root",
    publish                       := {},
    publishLocal                  := {},
    git.remoteRepo                := "git@github.com:scala-tessella/dcel.git",
    // Required so GitHub Pages doesn't run Jekyll, which would otherwise strip
    // out Scaladoc's underscore-prefixed asset directories.
    ghpagesNoJekyll               := true,
    // Publish the Scaladoc at the site root rather than under a subfolder.
    SiteScaladoc / siteSubdirName := "",
    // Use the JVM build's Scaladoc as canonical (JS produces the same docs).
    addMappingsToSiteDir(
      dcelJVM / Compile / packageDoc / mappings,
      SiteScaladoc / siteSubdirName
    )
  )
