## Question

Please explain and add scalafmt + scalafix, and optionally wartremover or more modern linters (Scalafix rules like DisableSyntax, LeakingImplicitClassVal).

## Answer

_By ChatGPT 5 on Sep 6, 2025_


Below is a short intro that explains what each tool does and why to use it, followed by the minimal, safe setup you can copy-paste. I’ve also included “how to run” and “how to disable per-file/per-warn” notes.

What these tools are and why they help
- Scalafmt (formatting)
    - Auto-formats Scala code consistently (spaces, line wrapping, alignment, etc.).
    - Improves readability and reduces diff noise in PRs.
- Scalafix (rewrites + linting)
    - Source code refactoring and linting framework.
    - Can automatically fix common issues (organize imports, add explicit types, etc.) or fail the build for patterns you want to prevent (DisableSyntax).
    - Works well on Scala 3 with semantic information enabled.
- Wartremover (linting, optional)
    - A rule-based linter that stops entire classes of bugs (e.g., disallow null, var, Option partial methods).
    - Use a conservative set for Scala 3; you can tune over time.

Add the tooling
1) sbt plugins
   Create or update project/plugins.sbt with these plugins.

```sbt
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.13.3")
// Optional, but recommended. Start strict and dial back if needed.
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "3.1.7")
```


2) Scalafmt configuration
   Create .scalafmt.conf at the project root.

```textmate
version = "3.8.1"

runner.dialect = scala3
maxColumn = 110
align.preset = most

newlines.source = keep
newlines.afterCurlyLambdaParams = multilineAlways
newlines.afterImplicitKWInVerticalMultiline = true

rewrite.rules = [RedundantBraces, RedundantParens, SortModifiers]
project.excludeFilters = [".*target/.*", "project/.*", "node_modules/.*"]
```


3) Scalafix configuration
   Create .scalafix.conf at the project root. This config uses:
- OrganizeImports (auto-fix)
- DisableSyntax (fail build on patterns)
- LeakingImplicitClassVal (warn)
  You can enable more rules as you go.

```
rules = [
  OrganizeImports,
  DisableSyntax,
  LeakingImplicitClassVal
]

DisableSyntax {
  keywords = [
    var,
    null,
    return
  ]
  noAsInstanceOf = true
  noIsInstanceOf = true
  noWhile = true
  noXml = true
}

OrganizeImports {
  groupedImports = Merge
  groupExplicitlyImportedImplicits = true
  removeUnused = true
  expandRelative = true
}
```


If you use the OrganizeImports rule, add the dependency below (it’s included in the build.sbt snippet).

4) Wire everything into your build
- Enable semanticdb for Scalafix semantic rules.
- Turn on format-on-compile (optional).
- Add a safe subset of wartremover rules (optional).
- Add an alias to lint/format everything in one go.

```sbt
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.2"

// Enable semanticdb for Scalafix (Scala 3)
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

// Make scalafix rules available (e.g., OrganizeImports)
ThisBuild / scalafixDependencies ++= List(
  "com.github.liancheng" %% "organize-imports" % "0.6.0"
)

// Optional: format on compile (can be noisy in PRs; turn off if you prefer manual runs)
ThisBuild / scalafmtOnCompile := true

// Optional: add a convenient alias to lint/format/test locally
addCommandAlias("qa", ";scalafmtAll;test:scalafmtAll;scalafixAll;test")

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
  // Optional: wartremover – start with a conservative set for Scala 3
  wartremoverErrors ++= Seq(
    wartremover.Wart.Any,
    wartremover.Wart.Null,
    wartremover.Wart.Var,
    wartremover.Wart.AsInstanceOf,
    wartremover.Wart.IsInstanceOf,
    wartremover.Wart.OptionPartial,
    wartremover.Wart.Throw
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
```


How to use
- First-time install
    - sbt reload
    - sbt scalafmtAll         # format every Scala source
    - sbt test:scalafmtAll
    - sbt scalafixAll         # run all configured Scalafix rules
- Local QA loop
    - sbt qa                  # runs scalafmtAll, test:scalafmtAll, scalafixAll, then test (via alias)
- On CI
    - Add these steps:
        - sbt scalafmtCheckAll test:scalafmtCheckAll
        - sbt scalafixAll       # or scalafix --check if you want to fail on un-fixed rewrites (see note below)
        - sbt test

Notes and tips
- Scalafix “check mode”
    - For CI to fail when code isn’t rewritten yet, use: sbt "scalafixAll --check"
- Silencing or scoping rules
    - Per-file: add scalafix:off / scalafix:on comments around a block you want to exclude temporarily.
    - Per-project or per-config: adjust .scalafix.conf or wartremoverErrors/Warn lists in build.sbt.
- Wartremover tuning
    - If you hit false positives (especially in Test), move some warts from wartremoverErrors to wartremoverWarnings, or use wartremoverExcluded to exclude generated sources or specific files/patterns.
- IDE integration
    - Most IDEs support scalafmt-on-save; with the sbt plugin enabled, IntelliJ will pick it up automatically. For Scalafix, there are IntelliJ integrations, but running scalafixAll via sbt is typically enough for team workflows.

If you’d like, I can also add a GitHub Actions workflow that runs scalafmtCheckAll/scalafixAll/test for both JVM and JS in matrix builds.

Name: AI Assistant