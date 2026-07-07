package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Localise the √3-family gap: the pure-hexagon set `{3.3.6.6;3.6.3.6;6.6.6}` sits at 0/2 on the gate. For
  * each bounded-V-realizable oracle cell, use the VALIDATED cut-and-feed pieces (`representFrame` — tested in
  * `CutFeedSpec`) to feed the cell's OWN exact cut at its wrap-safe circumference, then DECOMPOSE the
  * failure:
  *   - `graphForensics`: does the type-restricted growth from the cut even produce edges of ALL 3 target
  *     types? (coversTarget=false ⇒ a REACHABILITY/growth gap — no covering cycle can exist.)
  *   - `feedDebug` at escalating (maxNodes, maxLen, capPerNode, maxBand): nodes / cycles / closed / hasTarget
  *     — is the cell's cycle found as bounds relax (a budget issue) or never (structural)?
  *
  * Run: `…Set3HexProbe [maxV] [oracleMaxSize]`
  */
object Set3HexProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private val set3 = ts("3.3.6.6,3.6.3.6,6.6.6")

  def main(args: Array[String]): Unit =
    val maxV       = args.headOption.map(_.toInt).getOrElse(20)
    val oracleSize = args.lift(1).map(_.toInt).getOrElse(24)
    println(s"Set3HexProbe: set=${set3.map(_.mkString(".")).mkString("; ")} maxV=$maxV oracle=$oracleSize")

    val oracleCells = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(t => t._1 == 3 && t._2.toSet == set3)
    println(s"oracle cells of set3: ${oracleCells.size}")

    val bucket = BucketAssembly.enumerateBucket(set3, maxV, targetCount = oracleCells.size)
    println(s"bounded-V realized ops: ${bucket.ops.size} at maxV=$maxV")

    for cell <- oracleCells do
      val key = DelaneySymbols.canonicalKey(cell._3)
      println(s"\n=== cell key=${key.take(28)}… ===")
      bucket.ops.get(key) match
        case None     => println("  [no op] — bounded-V did not realize this cell (raise maxV)")
        case Some(op) =>
          ProfileAutomaton.representFrame(op) match
            case None        => println("  representFrame=None (not representable at a horizontal circumference)")
            case Some(frame) =>
              println(f"  c=${frame.c}  cLen(period)=${frame.cLen}%.3f  cutProfiles=${frame.profs.size}")
              // (1) does growth from the cut produce all 3 types?
              val forensics = ProfileAutomaton.graphForensics(frame.c, set3, frame.profs, maxNodes = 20000)
              println("  " +
                forensics.linesIterator.next()) // summary line: nodes / allEdgeTypes / coversTarget
              // (2) feed at escalating bounds × maxBand
              for
                (mn, ml, cap) <- List((12000, 48, 16), (30000, 72, 48), (60000, 96, 64))
                mb            <- List(1, 2, 3)
              do
                val d = ProfileAutomaton.feedDebug(frame.c, set3, frame.profs, key, mn, ml, cap, mb)
                println(f"    maxNodes=$mn%-6d maxLen=$ml%-3d cap=$cap%-3d maxBand=$mb  ->  $d")
    println("[done]")
