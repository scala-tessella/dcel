package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** The COMPLETE rotational-symmetry reference table for the n-uniform tilings (default n=2 → all 20), built
  * by the UNION of both sound engines, so it reaches every tiling:
  *   - bounded-V dart assembler → the small cells (incl. the rotation-free p1/pg/pm/cm ones the grower cannot
  *     reach), with a reduced state budget so the doomed large-cell buckets fail fast;
  *   - the symmetry grower (`symmetryRotationReference`) → the large rotational cells bounded-V can't afford.
  *
  * Both keyed in the shared `DelaneySymbols` D-symbol space, so the merge is a clean key union. Each distinct
  * tiling is a row: its vertex-type set and its rotation centres as (centre-type, angle) where m ↦ 360/m°.
  * Both phases print live progress (per-bucket / per-seed) — never a blind wait.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.UnionRotationTableProbe [n] [maxV] [maxFaces]`
  */
object UnionRotationTableProbe:

  private val angle = Map(2 -> "180°", 3 -> "120°", 4 -> "90°", 6 -> "60°")

  private def label(ts: Set[VertexSignature]): String =
    ts.map(_.mkString(".")).toList.sorted.mkString("; ")

  def main(args: Array[String]): Unit =
    val n         = args.headOption.map(_.toInt).getOrElse(2)
    val maxV      = args.lift(1).map(_.toInt).getOrElse(20)
    val maxFaces  = args.lift(2).map(_.toInt).getOrElse(52)
    val growerMin =
      args.lift(3).map(_.toLong).getOrElse(Long.MaxValue) // grower-phase wall-clock cap (minutes)
    val t0    = System.nanoTime()
    def secs  = (System.nanoTime() - t0) / 1e9
    // key -> (type-set, centres, source)
    val table =
      scala.collection.mutable.LinkedHashMap.empty[String, (Set[VertexSignature], Set[(String, Int)], String)]

    // ---- Phase A: bounded-V over each candidate type-set (reduced budget; doomed buckets fail fast) ----
    val sets   = UnionDriver.candidateTypeSets(n)
    val budget = 5_000_000L
    println(s"[union rotation table] n=$n maxV=$maxV maxFaces=$maxFaces — ${sets.size} type-sets")
    println(s"--- phase A: bounded-V (budget=$budget) ---")
    for (ts, i) <- sets.zipWithIndex do
      val b0   = System.nanoTime()
      val mult = UnionDriver.multiplicity(n, ts)
      val r    = BucketAssembly.enumerateBucket(ts, maxV, budget, targetCount = mult)
      var got  = 0
      r.keys.foreach: key =>
        KrotenheerdtTorusMapSearch.realizeCell(r.ops(key)).foreach: (faces, pv, pw) =>
          table.getOrElseUpdate(
            key,
            (ts, KrotenheerdtTorusMapSearch.rotationCenters(faces, pv, pw), "bounded-V")
          )
          got += 1
      println(
        f"  [${secs}%5.0fs] ${i + 1}%2d/${sets.size} ${label(ts)}%-38s found=$got/$mult states=${r.states} budgetHit=${r.budgetHit} (+${(System.nanoTime() -
            b0) / 1e9}%.0fs)"
      )
    println(f"  bounded-V reached ${table.size} distinct tilings (${secs}%.0fs)")

    // ---- Phase B: parallel symmetry grower (fills the large rotational cells bounded-V missed) ----
    println(s"--- phase B: parallel symmetry grower (maxFaces=$maxFaces) ---")
    val before = table.size
    val grower = KrotenheerdtTorusMapSearch.symmetryRotationReferenceParallel(
      maxN = n,
      maxFaces = maxFaces,
      maxMillis = if growerMin == Long.MaxValue then Long.MaxValue else growerMin * 60000L,
      log = msg => println(s"  $msg")
    )
    grower.foreach { case (key, (types, centres)) =>
      table.getOrElseUpdate(key, (types, centres, "grower"))
    }
    println(f"  grower added ${table.size - before} new tilings (had ${grower.size} keys) (${secs}%.0fs)")

    // ---- the table (only the n-uniform rows: an n-uniform tiling has exactly n distinct vertex types; the
    // grower run at maxN=n also re-derives lower-n tilings, which we drop here) ----
    val expected = TilingReference.counts(n)
    val rows     = table.toList.collect {
      case (_, (types, centres, src)) if types.size == n => (label(types), centres, src)
    }
    println(f"\n=== n=$n rotation-symmetry table: ${rows.size}/$expected tilings, ${secs}%.0fs ===")
    println(f"${"vertex-type set"}%-46s | rotation centres (centre-type : angle)            | src")
    println("-" * 116)
    rows
      .sortBy(t => (t._1, t._2.toString))
      .foreach: (lbl, centres, src) =>
        val cs = centres.toList
          .sortBy((kind, ord) => (kind, -ord))
          .map((kind, ord) => s"$kind ${angle.getOrElse(ord, s"$ord?")}")
          .mkString(", ")
        println(f"$lbl%-46s | $cs%-48s | $src")
    if expected - rows.size > 0 then
      println(
        s"\n(NOTE: ${expected - rows.size} short of the reference at maxV=$maxV maxFaces=$maxFaces — LOWER BOUND)"
      )

    // ---- POINT-GROUP SUMMARY (the strategic number: how many tilings are C2-ONLY = the hard p2/cmm zone where
    // reflections could earn a further 2x; max rotation order 3/4/6 ⇒ rotation-only is already >=3x and optimal
    // for the chiral groups) ----
    val byOrder = rows.map((_, centres, _) => if centres.isEmpty then 0 else centres.map(_._2).max)
    println(s"\n=== point-group summary over the ${rows.size} reached n=$n tilings ===")
    List(6, 4, 3, 2, 0).foreach: m =>
      val c   = byOrder.count(_ == m)
      val lbl = if m == 0 then "NO rotation (rotation-free!)" else s"max rotation order $m (${angle(m)})"
      if c > 0 || m == 0 then println(f"  $lbl%-34s : $c%2d")
    val c2only  = rows.filter((_, centres, _) => centres.nonEmpty && centres.map(_._2).max == 2)
    println(f"\n  C2-ONLY (the reflection-question zone): ${c2only.size}/${rows.size}")
    c2only.sortBy(_._1).foreach((lbl, centres, _) =>
      println(s"    $lbl  [${centres.toList.sorted.map((k, o) => s"$k$o").mkString(",")}]")
    )
    println("\n[done]")
