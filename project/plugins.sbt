addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.2")
// Scala Native plugins are intentionally commented out — see ADR-0007.
// Spire does not yet publish a Native artifact; uncomment both Native lines
// (here and the NativePlatform / dcelNative lines in build.sbt) once that unblocks.
//addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.11")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
//addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
addSbtPlugin("com.github.sbt" % "sbt-site" % "1.7.0")
addSbtPlugin("com.github.sbt" % "sbt-ghpages" % "0.9.0")
// Optional, but recommended. Start strict and dial back if needed.
//addSbtPlugin("org.wartremover" % "sbt-wartremover" % "3.4.0")
