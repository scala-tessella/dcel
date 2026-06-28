package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Run the VALIDATED cut-and-feed diagnostic ([[ProfileAutomaton.cutFeedDiagnose]], positive-controlled in
  * `CutFeedSpec`) on every n=3 gap cell, to localise WHY the missing banded cells elude the engine. Per cell:
  * representable — band axis is 30°-aligned (else a REPRESENTATION gap); fed — feeding the cell's own cut as
  * a seed, the engine emits its key (the trustworthy cut+grow+close signal). Cross-tabbed against engine
  * MATCHED/MISSING (the gate over the √3-aware circumferences).
  *
  * Run: `…CutFeedGapProbe [oracleMaxSize] [maxV] [maxNodes]`
  */
object CutFeedGapProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private val gaps = List(
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"),
    ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.4.4.6,3.6.3.6,4.4.4.4")
  )

  private val circs =
    List(
      ZetaPoint(2, 0, 0, 0),
      ZetaPoint(3, 0, 0, 0),
      ZetaPoint(4, 0, 0, 0),
      ZetaPoint(6, 0, 0, 0),
      ZetaPoint(0, 4, 0, -2),
      ZetaPoint(0, 6, 0, -3)
    )

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxV       = args.lift(1).map(_.toInt).getOrElse(16)
    val maxNodes   = args.lift(2).map(_.toInt).getOrElse(12000)
    println(s"CutFeedGapProbe: oracle=$oracleSize maxV=$maxV maxNodes=$maxNodes")

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    var repr, fed, miss = 0
    for t <- gaps do
      val cells  = oracle.filter(_._2.toSet == t)
      val engine = circs.flatMap(c => ProfileAutomaton.enumerateForTypeSetC(c, t, maxNodes).keySet).toSet
      val br     = BucketAssembly.enumerateBucket(t, maxV, targetCount = cells.size)
      println(s"\n=== ${t.map(_.mkString(".")).toList.sorted.mkString("; ")} (oracle=${cells.size}) ===")
      for c <- cells do
        val key = DelaneySymbols.canonicalKey(c._3)
        val got = engine.contains(key)
        br.ops.get(key) match
          case None     =>
            println(f"  [no op]                        engine=${if got then "MATCHED" else "missing"}")
          case Some(op) =>
            val r = ProfileAutomaton.cutFeedDiagnose(op, key, t, maxNodes)
            if !got then miss += 1
            if r.representable then repr += 1
            if r.fedEmitsKey then fed += 1
            println(f"  ${if got then "MATCHED" else "MISSING"}  repr=${r.representable}%-5s bandH=${
                r.bandAxisHorizontal
              }%-5s fed=${r.fedEmitsKey}%-5s  ${r.note}")
    println(f"\n=== TOTALS: missing=$miss | representable=$repr fed=$fed ===")
    println("[done]")
