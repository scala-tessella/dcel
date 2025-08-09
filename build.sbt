ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "dcel",
    idePackagePrefix := Some("io.github.scala_tessella"),
    libraryDependencies ++= Seq(
      "io.github.scala-tessella" %% "ring-seq" % "0.6.2",
      "org.typelevel" %% "spire" % "0.18.0",
      "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
