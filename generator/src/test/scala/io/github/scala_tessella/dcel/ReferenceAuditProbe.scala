package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** AUDIT `TilingReference` against our own SOUND, COMPLETE oracle for n ≤ 3 (the diagnosis found a
  * per-type-set count discrepancy: oracle says {3².6²;3.4².6;3.6.3.6} has 2 cells, the reference says 3). The
  * generate-all `DelaneySymbols` oracle is sound (no spurious cells) and complete for n ≤ 3 (reproduces
  * 11/20/39, stable at maxSize 24=26, R discharged) ⇒ it is the AUTHORITY; any per-type-set disagreement is a
  * reference (Wikipedia-transcription) error. Establishes the definitive n ≤ 3 reference (the oracle's
  * distribution) and lists every discrepancy so `TilingReference` can be corrected — which also de-risks the
  * type-set targeting the grower/oracle rely on (ADR-0036 fairness gap: stop trusting the transcribed list).
  *
  * Run: `…ReferenceAuditProbe [oracleMaxSize]` (default 26)
  */
object ReferenceAuditProbe:
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")

  def main(args: Array[String]): Unit =
    val maxSize = args.headOption.map(_.toInt).getOrElse(26)
    println(s"ReferenceAuditProbe: oracle vs TilingReference, n≤3, oracle maxSize=$maxSize")
    val oracle  = DelaneySymbols
      .enumerateSymbolsParallel(3, maxSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values
      .map(_.head)
      .toList

    var totalDisc = 0
    for n <- 1 to 3 do
      val cells   = oracle.filter(_._1 == n)
      val oMult   = cells.groupBy(_._2.toSet).view.mapValues(_.size).toMap
      val refSets = UnionDriver.candidateTypeSets(n)
      val rMult   = refSets.map(ts => ts -> UnionDriver.multiplicity(n, ts)).toMap
      val all     = (oMult.keySet ++ rMult.keySet).toList.sortBy(label)
      val oTot    = cells.size
      val rTot    = rMult.values.sum
      println(
        f"\n=== n=$n : ORACLE total $oTot, REFERENCE total $rTot (A068600=${TilingReference.counts(n)}) ==="
      )
      var disc    = 0
      for ts <- all do
        val o = oMult.getOrElse(ts, 0)
        val r = rMult.getOrElse(ts, 0)
        if o != r then
          disc += 1
          val tag =
            if r == 0 then "reference MISSING"
            else if o == 0 then "reference SPURIOUS (oracle has no such tiling)"
            else "count mismatch"
          println(f"  ⚠ oracle=$o reference=$r  [$tag]  ${label(ts)}")
      if disc == 0 then println("  ✓ oracle and reference agree on every type-set")
      else println(s"  → $disc discrepancy(ies) at n=$n")
      totalDisc += disc

    println(s"\n=== AUDIT SUMMARY: $totalDisc total discrepancy(ies) across n≤3 ===")
    println(
      "(oracle = sound+complete authority for n≤3 ⇒ each discrepancy is a TilingReference error to fix)"
    )
    println("[done]")
