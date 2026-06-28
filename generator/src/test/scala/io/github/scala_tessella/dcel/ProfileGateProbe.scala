package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** GATE for the profile-state engine ([[ProfileAutomaton]]): does the per-type-set covering-cycle finder
  * reach the oracle's n=3 cells of the 4 banded GAP type-sets (which contain the grower-missed cells)?
  * Compares the engine's emitted D-symbol keys to the oracle's, per gap type-set, over a small integer-`c`
  * sweep.
  *
  * Run: `…ProfileGateProbe [oracleMaxSize] [maxNodes] [maxLen] [c1,c2,…]`
  */
object ProfileGateProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private val gaps = List(
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"),
    ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.4.4.6,3.6.3.6,4.4.4.4")
  )

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxNodes   = args.lift(1).map(_.toInt).getOrElse(30000)
    val maxLen     = args.lift(2).map(_.toInt).getOrElse(48)
    val circs      = args.lift(3).map(_.split(',').map(_.toInt).toList).getOrElse(List(2, 4))
    println(s"ProfileGateProbe: oracle=$oracleSize maxNodes=$maxNodes maxLen=$maxLen circs=$circs")

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    for t <- gaps do
      val oracleKeys = oracle.filter(_._2.toSet == t).map(c => DelaneySymbols.canonicalKey(c._3)).toSet
      val engineKeys =
        circs.flatMap(c => ProfileAutomaton.enumerateForTypeSet(c, t, maxNodes, maxLen).keySet).toSet
      val matched    = oracleKeys & engineKeys
      val spurious   = engineKeys -- oracle.map(c => DelaneySymbols.canonicalKey(c._3)).toSet
      val label      = t.map(_.mkString(".")).toList.sorted.mkString("; ")
      println(
        f"  ${label}%-48s oracle=${oracleKeys.size} engine=${engineKeys.size} matched=${matched.size} spurious=${spurious.size}"
      )
    println("[done]")
