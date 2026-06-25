package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** DIAGNOSE the grower's TRUE n=3 gap against the (audit-corrected) oracle — the AUTHORITY for n ≤ 3. For
  * EVERY n=3 type-set the oracle produces (derived from the oracle, NOT read from the reference — ADR-0036),
  * run the grower constrained to it and compare reached vs oracle cells in the shared canonical-key space. For
  * each missed cell, print its INTRINSIC symmetry off the minimal D-symbol ([[DelaneySymbols.orbifoldSignature]]
  * = cone/rotation orders, orientability, mirrors; [[DelaneySymbols.hasRotation]]) so the gap's mechanism
  * (seed type / depth / closure) can be read. Replaces the earlier 3-hardcoded-deficit version (those counts
  * came from the reference, which the audit proved wrong).
  *
  * Run: `…GrowerGapDiagnosisProbe [oracleMaxSize] [growerMaxFaces] [growerPerSetMin]` (default 24 / 96 / 3)
  */
object GrowerGapDiagnosisProbe:
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxFaces   = args.lift(1).map(_.toInt).getOrElse(96)
    val perSetMs   = args.lift(2).map(_.toLong).getOrElse(3L) * 60000L

    println(
      s"GrowerGapDiagnosisProbe: oracle maxSize=$oracleSize, grower maxFaces=$maxFaces, ${perSetMs / 60000}min/set"
    )
    println("  enumerating the oracle (all n=3) ...")
    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values
      .map(_.head)
      .toList // dedup by canonical key
    val n3       = oracle.filter(_._1 == 3)
    val typeSets =
      n3.map(_._2.toSet).distinct.sortBy(label) // DERIVE type-sets from the oracle, not the reference
    println(s"  oracle n=3: ${n3.size} tilings across ${typeSets.size} type-sets")

    var totalReached = 0
    val gaps         = scala.collection.mutable.ListBuffer.empty[String]
    for t <- typeSets do
      val cells    = n3.filter(c => c._2.toSet == t)
      val reached  = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(
          3,
          maxFaces,
          parallelism = 12,
          maxMillis = perSetMs,
          targetTypes = t
        )
        .filter(_._2._1 == t)
        .keySet
      val cellKeys = cells.map(c => DelaneySymbols.canonicalKey(c._3)).toSet
      val got      = (reached intersect cellKeys).size
      val spurious = reached -- cellKeys
      totalReached += got
      val flag     = if got < cells.size then s"  GAP ${cells.size - got}" else ""
      println(f"  ${label(t)}%-52s oracle=${cells.size} grower=$got$flag" +
        (if spurious.nonEmpty then s"  ⚠spurious=${spurious.size}" else ""))
      if got < cells.size then
        cells.filterNot(c => reached.contains(DelaneySymbols.canonicalKey(c._3))).foreach: c =>
          val line =
            f"     MISSED ${DelaneySymbols.orbifoldSignature(c._3)}  hasRot=${DelaneySymbols.hasRotation(c._3)}"
          gaps += s"${label(t)} —$line"
          println(line)

    println(f"\n=== GROWER n=3 vs corrected oracle: reached $totalReached / ${n3.size} ===")
    println(s"true gaps (${gaps.size}):")
    gaps.foreach(g => println(s"  $g"))
    println("[done]")
