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
    val maxNodes   = args.lift(1).map(_.toInt).getOrElse(15000)
    val maxLen     = args.lift(2).map(_.toInt).getOrElse(48)
    // (b) AFFORDABLE-BAND MEASUREMENT: which maxBand values to sweep (how many band-height repeats to try).
    // Protected by ProfileAutomatonSpec T1/T2/T3 (band expansion fires, is monotone, and is sound).
    val bands      = args.lift(3).map(_.split(',').map(_.toInt).toList).getOrElse(List(1, 2, 3))
    // circumference sweep: integers 2,4 (period-1,2 cells) + √3-family 2√3 (hexagon cells, no edge-wrap; the
    // minimal √3 circumference above the hexagon's extent 2)
    val circs      = List(
      ("2", ZetaPoint(2, 0, 0, 0)),
      ("3", ZetaPoint(3, 0, 0, 0)),
      ("4", ZetaPoint(4, 0, 0, 0)),
      ("6", ZetaPoint(6, 0, 0, 0)),
      ("2√3", ZetaPoint(0, 4, 0, -2)),
      ("3√3", ZetaPoint(0, 6, 0, -3))
    )
    println(
      s"ProfileGateProbe: oracle=$oracleSize maxNodes=$maxNodes maxLen=$maxLen circs=${circs.map(_._1)}"
    )

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    val allOracleKeys = oracle.map(c => DelaneySymbols.canonicalKey(c._3)).toSet

    for mb <- bands do
      println(s"\n=== maxBand=$mb ===")
      val t0                    = System.nanoTime()
      var totMatched, totOracle = 0
      var totSpurious           = 0
      for t <- gaps do
        val oracleKeys = oracle.filter(_._2.toSet == t).map(c => DelaneySymbols.canonicalKey(c._3)).toSet
        val engineKeys = circs.flatMap((_, c) =>
          ProfileAutomaton.enumerateForTypeSetC(c, t, maxNodes, maxLen, maxBand = mb).keySet
        ).toSet
        val matched    = oracleKeys & engineKeys
        val spurious   = engineKeys -- allOracleKeys
        totMatched += matched.size; totOracle += oracleKeys.size; totSpurious += spurious.size
        val label      = t.map(_.mkString(".")).toList.sorted.mkString("; ")
        println(
          f"  ${label}%-48s oracle=${oracleKeys.size} engine=${engineKeys.size} matched=${matched.size} spurious=${spurious.size}"
        )
        System.out.flush()
      val secs                  = (System.nanoTime() - t0) / 1e9
      println(f"  --- maxBand=$mb TOTAL matched=$totMatched/$totOracle spurious=$totSpurious  (${secs}%.1fs)")
      System.out.flush()
    println("[done]")
