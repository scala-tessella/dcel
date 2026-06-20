package io.github.scala_tessella.dcel

/** Profiles [[DelaneySymbols.orientedRegularSymbols]] to locate the wall-clock bottleneck before optimizing.
  */
object OrbifoldProfileProbe:

  def main(args: Array[String]): Unit =
    val oriSize = args.headOption.map(_.toInt).getOrElse(28)
    val maxN    = args.lift(1).map(_.toInt).getOrElse(7)
    println(DelaneySymbols.orientedProfile(maxN, oriSize))
